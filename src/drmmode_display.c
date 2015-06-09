/*
 * Copyright © 2007 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Authors:
 *    Dave Airlie <airlied@redhat.com>
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <errno.h>
#include <sys/ioctl.h>
#include <time.h>
#include "cursorstr.h"
#include "damagestr.h"
#include "micmap.h"
#include "xf86cmap.h"
#include "sarea.h"

#include "drmmode_display.h"
#include "amdgpu_bo_helper.h"
#include "amdgpu_glamor.h"
#include "amdgpu_pixmap.h"

/* DPMS */
#ifdef HAVE_XEXTPROTO_71
#include <X11/extensions/dpmsconst.h>
#else
#define DPMS_SERVER
#include <X11/extensions/dpms.h>
#endif

#include <gbm.h>

#define DEFAULT_NOMINAL_FRAME_RATE 60

static Bool drmmode_xf86crtc_resize(ScrnInfoPtr scrn, int width, int height);

static Bool
AMDGPUZaphodStringMatches(ScrnInfoPtr pScrn, const char *s, char *output_name)
{
	int i = 0;
	char s1[20];

	do {
		switch (*s) {
		case ',':
			s1[i] = '\0';
			i = 0;
			if (strcmp(s1, output_name) == 0)
				return TRUE;
			break;
		case ' ':
		case '\t':
		case '\n':
		case '\r':
			break;
		default:
			s1[i] = *s;
			i++;
			break;
		}
	} while (*s++);

	s1[i] = '\0';
	if (strcmp(s1, output_name) == 0)
		return TRUE;

	return FALSE;
}

static PixmapPtr drmmode_create_bo_pixmap(ScrnInfoPtr pScrn,
					  int width, int height,
					  int depth, int bpp,
					  int pitch,
					  struct amdgpu_buffer *bo)
{
	ScreenPtr pScreen = pScrn->pScreen;
	PixmapPtr pixmap;

	pixmap = (*pScreen->CreatePixmap)(pScreen, 0, 0, depth,
					  AMDGPU_CREATE_PIXMAP_SCANOUT);
	if (!pixmap)
		return NULL;

	if (!(*pScreen->ModifyPixmapHeader) (pixmap, width, height,
					     depth, bpp, pitch, NULL)) {
		return NULL;
	}

	amdgpu_set_pixmap_bo(pixmap, bo);

	if (!amdgpu_glamor_create_textured_pixmap(pixmap,
						  amdgpu_get_pixmap_private(pixmap))) {
		pScreen->DestroyPixmap(pixmap);
		return NULL;
	}

	return pixmap;
}

static void drmmode_destroy_bo_pixmap(PixmapPtr pixmap)
{
	ScreenPtr pScreen = pixmap->drawable.pScreen;

	(*pScreen->DestroyPixmap) (pixmap);
}

static void
drmmode_ConvertFromKMode(ScrnInfoPtr scrn,
			 drmModeModeInfo * kmode, DisplayModePtr mode)
{
	memset(mode, 0, sizeof(DisplayModeRec));
	mode->status = MODE_OK;

	mode->Clock = kmode->clock;

	mode->HDisplay = kmode->hdisplay;
	mode->HSyncStart = kmode->hsync_start;
	mode->HSyncEnd = kmode->hsync_end;
	mode->HTotal = kmode->htotal;
	mode->HSkew = kmode->hskew;

	mode->VDisplay = kmode->vdisplay;
	mode->VSyncStart = kmode->vsync_start;
	mode->VSyncEnd = kmode->vsync_end;
	mode->VTotal = kmode->vtotal;
	mode->VScan = kmode->vscan;

	mode->Flags = kmode->flags;	//& FLAG_BITS;
	mode->name = strdup(kmode->name);

	if (kmode->type & DRM_MODE_TYPE_DRIVER)
		mode->type = M_T_DRIVER;
	if (kmode->type & DRM_MODE_TYPE_PREFERRED)
		mode->type |= M_T_PREFERRED;
	xf86SetModeCrtc(mode, scrn->adjustFlags);
}

static void
drmmode_ConvertToKMode(ScrnInfoPtr scrn,
		       drmModeModeInfo * kmode, DisplayModePtr mode)
{
	memset(kmode, 0, sizeof(*kmode));

	kmode->clock = mode->Clock;
	kmode->hdisplay = mode->HDisplay;
	kmode->hsync_start = mode->HSyncStart;
	kmode->hsync_end = mode->HSyncEnd;
	kmode->htotal = mode->HTotal;
	kmode->hskew = mode->HSkew;

	kmode->vdisplay = mode->VDisplay;
	kmode->vsync_start = mode->VSyncStart;
	kmode->vsync_end = mode->VSyncEnd;
	kmode->vtotal = mode->VTotal;
	kmode->vscan = mode->VScan;

	kmode->flags = mode->Flags;	//& FLAG_BITS;
	if (mode->name)
		strncpy(kmode->name, mode->name, DRM_DISPLAY_MODE_LEN);
	kmode->name[DRM_DISPLAY_MODE_LEN - 1] = 0;

}

/*
 * Retrieves present time in microseconds that is compatible
 * with units used by vblank timestamps. Depending on the kernel
 * version and DRM kernel module configuration, the vblank
 * timestamp can either be in real time or monotonic time
 */
int drmmode_get_current_ust(int drm_fd, CARD64 * ust)
{
	uint64_t cap_value;
	int ret;
	struct timespec now;

	ret = drmGetCap(drm_fd, DRM_CAP_TIMESTAMP_MONOTONIC, &cap_value);
	if (ret || !cap_value)
		/* old kernel or drm_timestamp_monotonic turned off */
		ret = clock_gettime(CLOCK_REALTIME, &now);
	else
		ret = clock_gettime(CLOCK_MONOTONIC, &now);
	if (ret)
		return ret;
	*ust = ((CARD64) now.tv_sec * 1000000) + ((CARD64) now.tv_nsec / 1000);
	return 0;
}

/*
 * Get current frame count and frame count timestamp of the crtc.
 */
int drmmode_crtc_get_ust_msc(xf86CrtcPtr crtc, CARD64 *ust, CARD64 *msc)
{
	ScrnInfoPtr scrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);
	drmVBlank vbl;
	int ret;

	vbl.request.type = DRM_VBLANK_RELATIVE;
	vbl.request.type |= amdgpu_populate_vbl_request_type(crtc);
	vbl.request.sequence = 0;

	ret = drmWaitVBlank(info->dri2.drm_fd, &vbl);
	if (ret) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
			   "get vblank counter failed: %s\n", strerror(errno));
		return ret;
	}

	*ust = ((CARD64)vbl.reply.tval_sec * 1000000) + vbl.reply.tval_usec;
	*msc = vbl.reply.sequence;

	return Success;
}

static void drmmode_crtc_dpms(xf86CrtcPtr crtc, int mode)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	ScrnInfoPtr scrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);
	CARD64 ust;
	int ret;

	if (drmmode_crtc->dpms_mode == DPMSModeOn && mode != DPMSModeOn) {
		drmVBlank vbl;

		/*
		 * On->Off transition: record the last vblank time,
		 * sequence number and frame period.
		 */
		vbl.request.type = DRM_VBLANK_RELATIVE;
		vbl.request.type |= amdgpu_populate_vbl_request_type(crtc);
		vbl.request.sequence = 0;
		ret = drmWaitVBlank(info->dri2.drm_fd, &vbl);
		if (ret)
			xf86DrvMsg(scrn->scrnIndex, X_ERROR,
				   "%s cannot get last vblank counter\n",
				   __func__);
		else {
			CARD64 seq = (CARD64) vbl.reply.sequence;
			CARD64 nominal_frame_rate, pix_in_frame;

			ust = ((CARD64) vbl.reply.tval_sec * 1000000) +
			    vbl.reply.tval_usec;
			drmmode_crtc->dpms_last_ust = ust;
			drmmode_crtc->dpms_last_seq = seq;
			nominal_frame_rate = crtc->mode.Clock;
			nominal_frame_rate *= 1000;
			pix_in_frame = crtc->mode.HTotal * crtc->mode.VTotal;
			if (nominal_frame_rate == 0 || pix_in_frame == 0)
				nominal_frame_rate = DEFAULT_NOMINAL_FRAME_RATE;
			else
				nominal_frame_rate /= pix_in_frame;
			drmmode_crtc->dpms_last_fps = nominal_frame_rate;
		}
	} else if (drmmode_crtc->dpms_mode != DPMSModeOn && mode == DPMSModeOn) {
		/*
		 * Off->On transition: calculate and accumulate the
		 * number of interpolated vblanks while we were in Off state
		 */
		ret = drmmode_get_current_ust(info->dri2.drm_fd, &ust);
		if (ret)
			xf86DrvMsg(scrn->scrnIndex, X_ERROR,
				   "%s cannot get current time\n", __func__);
		else if (drmmode_crtc->dpms_last_ust) {
			CARD64 time_elapsed, delta_seq;
			time_elapsed = ust - drmmode_crtc->dpms_last_ust;
			delta_seq = time_elapsed * drmmode_crtc->dpms_last_fps;
			delta_seq /= 1000000;
			drmmode_crtc->interpolated_vblanks += delta_seq;

		}
	}
	drmmode_crtc->dpms_mode = mode;
}

/* TODO: currently this function only clear the front buffer to zero */
/* Moving forward, we might to look into making the copy with glamor instead */
void drmmode_copy_fb(ScrnInfoPtr pScrn, drmmode_ptr drmmode)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	uint32_t size = pScrn->displayWidth * info->pixel_bytes * pScrn->virtualY;

	/* memset the bo */
	amdgpu_bo_map(pScrn, info->front_buffer);
	memset(info->front_buffer->cpu_ptr, 0x00, size);
}

static void
drmmode_crtc_scanout_destroy(drmmode_ptr drmmode,
                             struct drmmode_scanout *scanout)
{

	if (scanout->pixmap) {
		drmmode_destroy_bo_pixmap(scanout->pixmap);
		scanout->pixmap = NULL;
	}

	if (scanout->bo) {
		drmModeRmFB(drmmode->fd, scanout->fb_id);
		scanout->fb_id = 0;
		amdgpu_bo_unref(&scanout->bo);
		scanout->bo = NULL;
	}

	if (scanout->damage) {
		DamageDestroy(scanout->damage);
		scanout->damage = NULL;
	}
}

void
drmmode_scanout_free(ScrnInfoPtr scrn)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn);
	int c;

	for (c = 0; c < xf86_config->num_crtc; c++) {
		drmmode_crtc_private_ptr drmmode_crtc =
			xf86_config->crtc[c]->driver_private;

		drmmode_crtc_scanout_destroy(drmmode_crtc->drmmode,
					     &drmmode_crtc->scanout[0]);
		drmmode_crtc_scanout_destroy(drmmode_crtc->drmmode,
					     &drmmode_crtc->scanout[1]);
	}
}

static void *
drmmode_crtc_scanout_allocate(xf86CrtcPtr crtc,
                              struct drmmode_scanout *scanout,
                              int width, int height)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	int ret;
	int pitch;
	union gbm_bo_handle bo_handle;

	/* rotation requires acceleration */
	if (info->shadow_fb) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "Rotation requires acceleration!\n");
		return NULL;
	}

	if (scanout->bo) {
		if (scanout->width == width && scanout->height == height)
			return scanout->bo;

		drmmode_crtc_scanout_destroy(drmmode, scanout);
	}

	scanout->bo = amdgpu_alloc_pixmap_bo(pScrn, width, height,
					       pScrn->depth, 0,
					       pScrn->bitsPerPixel, &pitch);
	if (!scanout->bo) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
			   "Failed to allocate rotation buffer memory\n");
		return NULL;
	}

	bo_handle = gbm_bo_get_handle(scanout->bo->bo.gbm);
	ret = drmModeAddFB(drmmode->fd, width, height, pScrn->depth,
			   pScrn->bitsPerPixel, pitch,
			   bo_handle.u32, &scanout->fb_id);
	if (ret) {
		ErrorF("failed to add rotate fb\n");
	}

	scanout->width = width;
	scanout->height = height;
	return scanout->bo;
}

static PixmapPtr
drmmode_crtc_scanout_create(xf86CrtcPtr crtc,
                            struct drmmode_scanout *scanout,
                            void *data, int width, int height)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	unsigned long rotate_pitch;

	if (scanout->pixmap) {
		if (scanout->width == width && scanout->height == height)
			return scanout->pixmap;

		drmmode_crtc_scanout_destroy(drmmode, scanout);
	}

	if (!data)
		data = drmmode_crtc_scanout_allocate(crtc, scanout, width, height);

	rotate_pitch = gbm_bo_get_stride(scanout->bo->bo.gbm);

	scanout->pixmap = drmmode_create_bo_pixmap(pScrn,
						 width, height,
						 pScrn->depth,
						 pScrn->bitsPerPixel,
						 rotate_pitch,
						 scanout->bo);
	if (scanout->pixmap == NULL) {
		xf86DrvMsg(pScrn->scrnIndex, X_ERROR,
		           "Couldn't allocate shadow pixmap for rotated CRTC\n");
	}
	return scanout->pixmap;

}

static void
amdgpu_screen_damage_report(DamagePtr damage, RegionPtr region, void *closure)
{
	/* Only keep track of the extents */
	RegionUninit(&damage->damage);
	damage->damage.data = NULL;
}

static Bool
drmmode_set_mode_major(xf86CrtcPtr crtc, DisplayModePtr mode,
		       Rotation rotation, int x, int y)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(crtc->scrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	int saved_x, saved_y;
	Rotation saved_rotation;
	DisplayModeRec saved_mode;
	uint32_t *output_ids;
	int output_count = 0;
	Bool ret = TRUE;
	int i;
	int fb_id;
	drmModeModeInfo kmode;
	uint32_t bo_handle;

	if (drmmode->fb_id == 0) {
		if (!amdgpu_bo_get_handle(info->front_buffer, &bo_handle)) {
			ErrorF("failed to get BO handle for FB\n");
			return FALSE;
		}

		ret = drmModeAddFB(drmmode->fd,
				   pScrn->virtualX,
				   pScrn->virtualY,
				   pScrn->depth, pScrn->bitsPerPixel,
				   pScrn->displayWidth * info->pixel_bytes,
				   bo_handle, &drmmode->fb_id);
		if (ret < 0) {
			ErrorF("failed to add fb\n");
			return FALSE;
		}
	}

	saved_mode = crtc->mode;
	saved_x = crtc->x;
	saved_y = crtc->y;
	saved_rotation = crtc->rotation;

	if (mode) {
		crtc->mode = *mode;
		crtc->x = x;
		crtc->y = y;
		crtc->rotation = rotation;
		crtc->transformPresent = FALSE;
	}

	output_ids = calloc(sizeof(uint32_t), xf86_config->num_output);
	if (!output_ids) {
		ret = FALSE;
		goto done;
	}

	if (mode) {
		ScreenPtr pScreen = pScrn->pScreen;

		for (i = 0; i < xf86_config->num_output; i++) {
			xf86OutputPtr output = xf86_config->output[i];
			drmmode_output_private_ptr drmmode_output;

			if (output->crtc != crtc)
				continue;

			drmmode_output = output->driver_private;
			output_ids[output_count] =
			    drmmode_output->mode_output->connector_id;
			output_count++;
		}

		if (!xf86CrtcRotate(crtc)) {
			goto done;
		}
		crtc->funcs->gamma_set(crtc, crtc->gamma_red, crtc->gamma_green,
				       crtc->gamma_blue, crtc->gamma_size);

		drmmode_ConvertToKMode(crtc->scrn, &kmode, mode);

		fb_id = drmmode->fb_id;
#ifdef AMDGPU_PIXMAP_SHARING
		if (crtc->randr_crtc && crtc->randr_crtc->scanout_pixmap) {
			x = drmmode_crtc->prime_pixmap_x;
			y = 0;

			drmmode_crtc_scanout_destroy(drmmode, &drmmode_crtc->scanout[0]);
			drmmode_crtc_scanout_destroy(drmmode, &drmmode_crtc->scanout[1]);
		} else
#endif
		if (drmmode_crtc->rotate.fb_id) {
			fb_id = drmmode_crtc->rotate.fb_id;
			x = y = 0;

			drmmode_crtc_scanout_destroy(drmmode, &drmmode_crtc->scanout[0]);
			drmmode_crtc_scanout_destroy(drmmode, &drmmode_crtc->scanout[1]);
		} else if (info->tear_free || info->shadow_primary) {
			for (i = 0; i < (info->tear_free ? 2 : 1); i++) {
				drmmode_crtc_scanout_create(crtc,
							    &drmmode_crtc->scanout[i],
							    NULL, mode->HDisplay,
							    mode->VDisplay);

				if (drmmode_crtc->scanout[i].pixmap) {
					RegionPtr pRegion;
					BoxPtr pBox;

					if (!drmmode_crtc->scanout[i].damage) {
						drmmode_crtc->scanout[i].damage =
							DamageCreate(amdgpu_screen_damage_report,
								     NULL, DamageReportRawRegion,
								     TRUE, pScreen, NULL);
						DamageRegister(&pScreen->GetScreenPixmap(pScreen)->drawable,
							       drmmode_crtc->scanout[i].damage);
					}

					pRegion = DamageRegion(drmmode_crtc->scanout[i].damage);
					RegionUninit(pRegion);
					pRegion->data = NULL;
					pBox = RegionExtents(pRegion);
					pBox->x1 = min(pBox->x1, x);
					pBox->y1 = min(pBox->y1, y);
					pBox->x2 = max(pBox->x2, x + mode->HDisplay);
					pBox->y2 = max(pBox->y2, y + mode->VDisplay);
				}
			}

			if (drmmode_crtc->scanout[0].pixmap &&
			    (!info->tear_free || drmmode_crtc->scanout[1].pixmap)) {
				drmmode_crtc->scanout_id = 0;
				fb_id = drmmode_crtc->scanout[0].fb_id;
				x = y = 0;
			}
		}
		ret =
		    drmModeSetCrtc(drmmode->fd,
				   drmmode_crtc->mode_crtc->crtc_id, fb_id, x,
				   y, output_ids, output_count, &kmode);
		if (ret)
			xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
				   "failed to set mode: %s", strerror(-ret));
		else
			ret = TRUE;

		if (crtc->scrn->pScreen)
			xf86CrtcSetScreenSubpixelOrder(crtc->scrn->pScreen);
		/* go through all the outputs and force DPMS them back on? */
		for (i = 0; i < xf86_config->num_output; i++) {
			xf86OutputPtr output = xf86_config->output[i];

			if (output->crtc != crtc)
				continue;

			output->funcs->dpms(output, DPMSModeOn);
		}
	}

	if (pScrn->pScreen &&
	    !xf86ReturnOptValBool(info->Options, OPTION_SW_CURSOR, FALSE))
		xf86_reload_cursors(pScrn->pScreen);

done:
	if (!ret) {
		crtc->x = saved_x;
		crtc->y = saved_y;
		crtc->rotation = saved_rotation;
		crtc->mode = saved_mode;
	}
#if defined(XF86_CRTC_VERSION) && XF86_CRTC_VERSION >= 3
	else
		crtc->active = TRUE;
#endif

	return ret;
}

static void drmmode_set_cursor_colors(xf86CrtcPtr crtc, int bg, int fg)
{

}

static void drmmode_set_cursor_position(xf86CrtcPtr crtc, int x, int y)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;

	drmModeMoveCursor(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id, x, y);
}

static void drmmode_load_cursor_argb(xf86CrtcPtr crtc, CARD32 * image)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	int i;
	uint32_t cursor_size = info->cursor_w * info->cursor_h;

	if (info->gbm) {
		uint32_t ptr[cursor_size];

		for (i = 0; i < cursor_size; i++)
			ptr[i] = cpu_to_le32(image[i]);

		gbm_bo_write(drmmode_crtc->cursor_buffer->bo.gbm, ptr, cursor_size * 4);
	} else {
		/* cursor should be mapped already */
		uint32_t *ptr = (uint32_t *) (drmmode_crtc->cursor_buffer->cpu_ptr);

		for (i = 0; i < cursor_size; i++)
			ptr[i] = cpu_to_le32(image[i]);
	}
}

static void drmmode_hide_cursor(xf86CrtcPtr crtc)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;

	drmModeSetCursor(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id, 0,
			 info->cursor_w, info->cursor_h);

}

static void drmmode_show_cursor(xf86CrtcPtr crtc)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	uint32_t bo_handle;

	if (!amdgpu_bo_get_handle(drmmode_crtc->cursor_buffer, &bo_handle)) {
		ErrorF("failed to get BO handle for cursor\n");
		return;
	}

	drmModeSetCursor(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id, bo_handle,
			 info->cursor_w, info->cursor_h);
}

static void *drmmode_crtc_shadow_allocate(xf86CrtcPtr crtc, int width,
					  int height)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;

	return drmmode_crtc_scanout_allocate(crtc, &drmmode_crtc->rotate,
					     width, height);
}

static PixmapPtr
drmmode_crtc_shadow_create(xf86CrtcPtr crtc, void *data, int width, int height)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;

	return drmmode_crtc_scanout_create(crtc, &drmmode_crtc->rotate, data,
					   width, height);
}

static void
drmmode_crtc_shadow_destroy(xf86CrtcPtr crtc, PixmapPtr rotate_pixmap,
			    void *data)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;

	drmmode_crtc_scanout_destroy(drmmode, &drmmode_crtc->rotate);
}

static void
drmmode_crtc_gamma_set(xf86CrtcPtr crtc, uint16_t * red, uint16_t * green,
		       uint16_t * blue, int size)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;

	drmModeCrtcSetGamma(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id,
			    size, red, green, blue);
}

#ifdef AMDGPU_PIXMAP_SHARING
static Bool drmmode_set_scanout_pixmap(xf86CrtcPtr crtc, PixmapPtr ppix)
{
	ScreenPtr screen = xf86ScrnToScreen(crtc->scrn);
	PixmapPtr screenpix = screen->GetScreenPixmap(screen);
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(crtc->scrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	int c, total_width = 0, max_height = 0, this_x = 0;

	if (!ppix) {
		if (crtc->randr_crtc->scanout_pixmap)
			PixmapStopDirtyTracking(crtc->
						randr_crtc->scanout_pixmap,
						screenpix);
		drmmode_crtc->prime_pixmap_x = 0;
		return TRUE;
	}

	/* iterate over all the attached crtcs -
	   work out bounding box */
	for (c = 0; c < xf86_config->num_crtc; c++) {
		xf86CrtcPtr iter = xf86_config->crtc[c];
		if (!iter->enabled && iter != crtc)
			continue;
		if (iter == crtc) {
			this_x = total_width;
			total_width += ppix->drawable.width;
			if (max_height < ppix->drawable.height)
				max_height = ppix->drawable.height;
		} else {
			total_width += iter->mode.HDisplay;
			if (max_height < iter->mode.VDisplay)
				max_height = iter->mode.VDisplay;
		}
#ifndef HAS_DIRTYTRACKING2
		if (iter != crtc) {
			ErrorF
			    ("Cannot do multiple crtcs without X server dirty tracking 2 interface\n");
			return FALSE;
		}
#endif
	}

	if (total_width != screenpix->drawable.width ||
	    max_height != screenpix->drawable.height) {
		Bool ret;
		ret =
		    drmmode_xf86crtc_resize(crtc->scrn, total_width,
					    max_height);
		if (ret == FALSE)
			return FALSE;

		screenpix = screen->GetScreenPixmap(screen);
		screen->width = screenpix->drawable.width = total_width;
		screen->height = screenpix->drawable.height = max_height;
	}
	drmmode_crtc->prime_pixmap_x = this_x;
#ifdef HAS_DIRTYTRACKING2
	PixmapStartDirtyTracking2(ppix, screenpix, 0, 0, this_x, 0);
#else
	PixmapStartDirtyTracking(ppix, screenpix, 0, 0);
#endif
	return TRUE;
}
#endif

static const xf86CrtcFuncsRec drmmode_crtc_funcs = {
	.dpms = drmmode_crtc_dpms,
	.set_mode_major = drmmode_set_mode_major,
	.set_cursor_colors = drmmode_set_cursor_colors,
	.set_cursor_position = drmmode_set_cursor_position,
	.show_cursor = drmmode_show_cursor,
	.hide_cursor = drmmode_hide_cursor,
	.load_cursor_argb = drmmode_load_cursor_argb,

	.gamma_set = drmmode_crtc_gamma_set,
	.shadow_create = drmmode_crtc_shadow_create,
	.shadow_allocate = drmmode_crtc_shadow_allocate,
	.shadow_destroy = drmmode_crtc_shadow_destroy,
	.destroy = NULL,	/* XXX */
#ifdef AMDGPU_PIXMAP_SHARING
	.set_scanout_pixmap = drmmode_set_scanout_pixmap,
#endif
};

int drmmode_get_crtc_id(xf86CrtcPtr crtc)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	return drmmode_crtc->hw_id;
}

void drmmode_crtc_hw_id(xf86CrtcPtr crtc)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	ScrnInfoPtr pScrn = crtc->scrn;
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	int r;

	r = amdgpu_query_crtc_from_id(pAMDGPUEnt->pDev,
				      drmmode_crtc->mode_crtc->crtc_id,
				      &drmmode_crtc->hw_id);
	if (r)
		drmmode_crtc->hw_id = -1;
}

static void drmmode_crtc_init(ScrnInfoPtr pScrn, drmmode_ptr drmmode, int num)
{
	xf86CrtcPtr crtc;
	drmmode_crtc_private_ptr drmmode_crtc;

	crtc = xf86CrtcCreate(pScrn, &drmmode_crtc_funcs);
	if (crtc == NULL)
		return;

	drmmode_crtc = xnfcalloc(sizeof(drmmode_crtc_private_rec), 1);
	drmmode_crtc->mode_crtc =
	    drmModeGetCrtc(drmmode->fd, drmmode->mode_res->crtcs[num]);
	drmmode_crtc->drmmode = drmmode;
	crtc->driver_private = drmmode_crtc;
	drmmode_crtc_hw_id(crtc);

	return;
}

static xf86OutputStatus drmmode_output_detect(xf86OutputPtr output)
{
	/* go to the hw and retrieve a new output struct */
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	xf86OutputStatus status;
	drmModeFreeConnector(drmmode_output->mode_output);

	drmmode_output->mode_output =
	    drmModeGetConnector(drmmode->fd, drmmode_output->output_id);
	if (!drmmode_output->mode_output)
		return XF86OutputStatusDisconnected;

	switch (drmmode_output->mode_output->connection) {
	case DRM_MODE_CONNECTED:
		status = XF86OutputStatusConnected;
		break;
	case DRM_MODE_DISCONNECTED:
		status = XF86OutputStatusDisconnected;
		break;
	default:
	case DRM_MODE_UNKNOWNCONNECTION:
		status = XF86OutputStatusUnknown;
		break;
	}
	return status;
}

static Bool
drmmode_output_mode_valid(xf86OutputPtr output, DisplayModePtr pModes)
{
	return MODE_OK;
}

static DisplayModePtr drmmode_output_get_modes(xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmModeConnectorPtr koutput = drmmode_output->mode_output;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	int i;
	DisplayModePtr Modes = NULL, Mode;
	drmModePropertyPtr props;
	xf86MonPtr mon = NULL;

	if (!koutput)
		return NULL;

	/* look for an EDID property */
	for (i = 0; i < koutput->count_props; i++) {
		props = drmModeGetProperty(drmmode->fd, koutput->props[i]);
		if (props && (props->flags & DRM_MODE_PROP_BLOB)) {
			if (!strcmp(props->name, "EDID")) {
				if (drmmode_output->edid_blob)
					drmModeFreePropertyBlob
					    (drmmode_output->edid_blob);
				drmmode_output->edid_blob =
				    drmModeGetPropertyBlob(drmmode->fd,
							   koutput->prop_values
							   [i]);
			}
		}
		if (props)
			drmModeFreeProperty(props);
	}

	if (drmmode_output->edid_blob) {
		mon = xf86InterpretEDID(output->scrn->scrnIndex,
					drmmode_output->edid_blob->data);
		if (mon && drmmode_output->edid_blob->length > 128)
			mon->flags |= MONITOR_EDID_COMPLETE_RAWDATA;
	}
	xf86OutputSetEDID(output, mon);

	/* modes should already be available */
	for (i = 0; i < koutput->count_modes; i++) {
		Mode = xnfalloc(sizeof(DisplayModeRec));

		drmmode_ConvertFromKMode(output->scrn, &koutput->modes[i],
					 Mode);
		Modes = xf86ModesAdd(Modes, Mode);

	}
	return Modes;
}

static void drmmode_output_destroy(xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	int i;

	if (drmmode_output->edid_blob)
		drmModeFreePropertyBlob(drmmode_output->edid_blob);
	for (i = 0; i < drmmode_output->num_props; i++) {
		drmModeFreeProperty(drmmode_output->props[i].mode_prop);
		free(drmmode_output->props[i].atoms);
	}
	for (i = 0; i < drmmode_output->mode_output->count_encoders; i++) {
		drmModeFreeEncoder(drmmode_output->mode_encoders[i]);
		free(drmmode_output->mode_encoders);
	}
	free(drmmode_output->props);
	drmModeFreeConnector(drmmode_output->mode_output);
	free(drmmode_output);
	output->driver_private = NULL;
}

static void drmmode_output_dpms(xf86OutputPtr output, int mode)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmModeConnectorPtr koutput = drmmode_output->mode_output;
	drmmode_ptr drmmode = drmmode_output->drmmode;

	drmModeConnectorSetProperty(drmmode->fd, koutput->connector_id,
				    drmmode_output->dpms_enum_id, mode);
	return;
}

static Bool drmmode_property_ignore(drmModePropertyPtr prop)
{
	if (!prop)
		return TRUE;
	/* ignore blob prop */
	if (prop->flags & DRM_MODE_PROP_BLOB)
		return TRUE;
	/* ignore standard property */
	if (!strcmp(prop->name, "EDID") || !strcmp(prop->name, "DPMS"))
		return TRUE;

	return FALSE;
}

static void drmmode_output_create_resources(xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmModeConnectorPtr mode_output = drmmode_output->mode_output;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	drmModePropertyPtr drmmode_prop;
	int i, j, err;

	drmmode_output->props =
	    calloc(mode_output->count_props, sizeof(drmmode_prop_rec));
	if (!drmmode_output->props)
		return;

	drmmode_output->num_props = 0;
	for (i = 0, j = 0; i < mode_output->count_props; i++) {
		drmmode_prop =
		    drmModeGetProperty(drmmode->fd, mode_output->props[i]);
		if (drmmode_property_ignore(drmmode_prop)) {
			drmModeFreeProperty(drmmode_prop);
			continue;
		}
		drmmode_output->props[j].mode_prop = drmmode_prop;
		drmmode_output->props[j].value = mode_output->prop_values[i];
		drmmode_output->num_props++;
		j++;
	}

	for (i = 0; i < drmmode_output->num_props; i++) {
		drmmode_prop_ptr p = &drmmode_output->props[i];
		drmmode_prop = p->mode_prop;

		if (drmmode_prop->flags & DRM_MODE_PROP_RANGE) {
			INT32 range[2];
			INT32 value = p->value;

			p->num_atoms = 1;
			p->atoms = calloc(p->num_atoms, sizeof(Atom));
			if (!p->atoms)
				continue;
			p->atoms[0] =
			    MakeAtom(drmmode_prop->name,
				     strlen(drmmode_prop->name), TRUE);
			range[0] = drmmode_prop->values[0];
			range[1] = drmmode_prop->values[1];
			err =
			    RRConfigureOutputProperty(output->randr_output,
						      p->atoms[0], FALSE, TRUE,
						      drmmode_prop->flags &
						      DRM_MODE_PROP_IMMUTABLE ?
						      TRUE : FALSE, 2, range);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
					   "RRConfigureOutputProperty error, %d\n",
					   err);
			}
			err =
			    RRChangeOutputProperty(output->randr_output,
						   p->atoms[0], XA_INTEGER, 32,
						   PropModeReplace, 1, &value,
						   FALSE, TRUE);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
					   "RRChangeOutputProperty error, %d\n",
					   err);
			}
		} else if (drmmode_prop->flags & DRM_MODE_PROP_ENUM) {
			p->num_atoms = drmmode_prop->count_enums + 1;
			p->atoms = calloc(p->num_atoms, sizeof(Atom));
			if (!p->atoms)
				continue;
			p->atoms[0] =
			    MakeAtom(drmmode_prop->name,
				     strlen(drmmode_prop->name), TRUE);
			for (j = 1; j <= drmmode_prop->count_enums; j++) {
				struct drm_mode_property_enum *e =
				    &drmmode_prop->enums[j - 1];
				p->atoms[j] =
				    MakeAtom(e->name, strlen(e->name), TRUE);
			}
			err =
			    RRConfigureOutputProperty(output->randr_output,
						      p->atoms[0], FALSE, FALSE,
						      drmmode_prop->flags &
						      DRM_MODE_PROP_IMMUTABLE ?
						      TRUE : FALSE,
						      p->num_atoms - 1,
						      (INT32 *) & p->atoms[1]);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
					   "RRConfigureOutputProperty error, %d\n",
					   err);
			}
			for (j = 0; j < drmmode_prop->count_enums; j++)
				if (drmmode_prop->enums[j].value == p->value)
					break;
			/* there's always a matching value */
			err =
			    RRChangeOutputProperty(output->randr_output,
						   p->atoms[0], XA_ATOM, 32,
						   PropModeReplace, 1,
						   &p->atoms[j + 1], FALSE,
						   TRUE);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
					   "RRChangeOutputProperty error, %d\n",
					   err);
			}
		}
	}
}

static Bool
drmmode_output_set_property(xf86OutputPtr output, Atom property,
			    RRPropertyValuePtr value)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	int i;

	for (i = 0; i < drmmode_output->num_props; i++) {
		drmmode_prop_ptr p = &drmmode_output->props[i];

		if (p->atoms[0] != property)
			continue;

		if (p->mode_prop->flags & DRM_MODE_PROP_RANGE) {
			uint32_t val;

			if (value->type != XA_INTEGER || value->format != 32 ||
			    value->size != 1)
				return FALSE;
			val = *(uint32_t *) value->data;

			drmModeConnectorSetProperty(drmmode->fd,
						    drmmode_output->output_id,
						    p->mode_prop->prop_id,
						    (uint64_t) val);
			return TRUE;
		} else if (p->mode_prop->flags & DRM_MODE_PROP_ENUM) {
			Atom atom;
			const char *name;
			int j;

			if (value->type != XA_ATOM || value->format != 32
			    || value->size != 1)
				return FALSE;
			memcpy(&atom, value->data, 4);
			name = NameForAtom(atom);

			/* search for matching name string, then set its value down */
			for (j = 0; j < p->mode_prop->count_enums; j++) {
				if (!strcmp(p->mode_prop->enums[j].name, name)) {
					drmModeConnectorSetProperty(drmmode->fd,
								    drmmode_output->output_id,
								    p->mode_prop->prop_id,
								    p->mode_prop->enums
								    [j].value);
					return TRUE;
				}
			}
		}
	}

	return TRUE;
}

static Bool drmmode_output_get_property(xf86OutputPtr output, Atom property)
{
	return TRUE;
}

static const xf86OutputFuncsRec drmmode_output_funcs = {
	.dpms = drmmode_output_dpms,
	.create_resources = drmmode_output_create_resources,
	.set_property = drmmode_output_set_property,
	.get_property = drmmode_output_get_property,
#if 0

	.save = drmmode_crt_save,
	.restore = drmmode_crt_restore,
	.mode_fixup = drmmode_crt_mode_fixup,
	.prepare = drmmode_output_prepare,
	.mode_set = drmmode_crt_mode_set,
	.commit = drmmode_output_commit,
#endif
	.detect = drmmode_output_detect,
	.mode_valid = drmmode_output_mode_valid,

	.get_modes = drmmode_output_get_modes,
	.destroy = drmmode_output_destroy
};

static int subpixel_conv_table[7] = { 0, SubPixelUnknown,
	SubPixelHorizontalRGB,
	SubPixelHorizontalBGR,
	SubPixelVerticalRGB,
	SubPixelVerticalBGR,
	SubPixelNone
};

const char *output_names[] = { "None",
	"VGA",
	"DVI-I",
	"DVI-D",
	"DVI-A",
	"Composite",
	"S-video",
	"LVDS",
	"CTV",
	"DIN",
	"DisplayPort",
	"HDMI-A",
	"HDMI-B",
	"TV",
	"eDP",
	"Virtual",
	"DSI",
};

#define NUM_OUTPUT_NAMES (sizeof(output_names) / sizeof(output_names[0]))

static void
drmmode_output_init(ScrnInfoPtr pScrn, drmmode_ptr drmmode, int num)
{
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);
	xf86OutputPtr output;
	drmModeConnectorPtr koutput;
	drmModeEncoderPtr *kencoders = NULL;
	drmmode_output_private_ptr drmmode_output;
	drmModePropertyPtr props;
	char name[32];
	int i;
	const char *s;

	koutput =
	    drmModeGetConnector(drmmode->fd,
				drmmode->mode_res->connectors[num]);
	if (!koutput)
		return;

	kencoders = calloc(sizeof(drmModeEncoderPtr), koutput->count_encoders);
	if (!kencoders) {
		goto out_free_encoders;
	}

	for (i = 0; i < koutput->count_encoders; i++) {
		kencoders[i] =
		    drmModeGetEncoder(drmmode->fd, koutput->encoders[i]);
		if (!kencoders[i]) {
			goto out_free_encoders;
		}
	}

	if (koutput->connector_type >= NUM_OUTPUT_NAMES)
		snprintf(name, 32, "Unknown%d-%d", koutput->connector_type,
			 koutput->connector_type_id - 1);
#ifdef AMDGPU_PIXMAP_SHARING
	else if (pScrn->is_gpu)
		snprintf(name, 32, "%s-%d-%d",
			 output_names[koutput->connector_type],
			 pScrn->scrnIndex - GPU_SCREEN_OFFSET + 1,
			 koutput->connector_type_id - 1);
#endif
	else
		snprintf(name, 32, "%s-%d",
			 output_names[koutput->connector_type],
			 koutput->connector_type_id - 1);

	if (xf86IsEntityShared(pScrn->entityList[0])) {
		if ((s =
		     xf86GetOptValString(info->Options, OPTION_ZAPHOD_HEADS))) {
			if (!AMDGPUZaphodStringMatches(pScrn, s, name))
				goto out_free_encoders;
		} else {
			if (info->IsPrimary && (num != 0))
				goto out_free_encoders;
			else if (info->IsSecondary && (num != 1))
				goto out_free_encoders;
		}
	}

	output = xf86OutputCreate(pScrn, &drmmode_output_funcs, name);
	if (!output) {
		goto out_free_encoders;
	}

	drmmode_output = calloc(sizeof(drmmode_output_private_rec), 1);
	if (!drmmode_output) {
		xf86OutputDestroy(output);
		goto out_free_encoders;
	}

	drmmode_output->output_id = drmmode->mode_res->connectors[num];
	drmmode_output->mode_output = koutput;
	drmmode_output->mode_encoders = kencoders;
	drmmode_output->drmmode = drmmode;
	output->mm_width = koutput->mmWidth;
	output->mm_height = koutput->mmHeight;

	output->subpixel_order = subpixel_conv_table[koutput->subpixel];
	output->interlaceAllowed = TRUE;
	output->doubleScanAllowed = TRUE;
	output->driver_private = drmmode_output;

	output->possible_crtcs = 0xffffffff;
	for (i = 0; i < koutput->count_encoders; i++) {
		output->possible_crtcs &= kencoders[i]->possible_crtcs;
	}
	/* work out the possible clones later */
	output->possible_clones = 0;

	for (i = 0; i < koutput->count_props; i++) {
		props = drmModeGetProperty(drmmode->fd, koutput->props[i]);
		if (props && (props->flags & DRM_MODE_PROP_ENUM)) {
			if (!strcmp(props->name, "DPMS")) {
				drmmode_output->dpms_enum_id =
				    koutput->props[i];
				drmModeFreeProperty(props);
				break;
			}
			drmModeFreeProperty(props);
		}
	}

	return;
out_free_encoders:
	if (kencoders) {
		for (i = 0; i < koutput->count_encoders; i++)
			drmModeFreeEncoder(kencoders[i]);
		free(kencoders);
	}
	drmModeFreeConnector(koutput);

}

uint32_t find_clones(ScrnInfoPtr scrn, xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output =
	    output->driver_private, clone_drmout;
	int i;
	xf86OutputPtr clone_output;
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn);
	int index_mask = 0;

	if (drmmode_output->enc_clone_mask == 0)
		return index_mask;

	for (i = 0; i < xf86_config->num_output; i++) {
		clone_output = xf86_config->output[i];
		clone_drmout = clone_output->driver_private;
		if (output == clone_output)
			continue;

		if (clone_drmout->enc_mask == 0)
			continue;
		if (drmmode_output->enc_clone_mask == clone_drmout->enc_mask)
			index_mask |= (1 << i);
	}
	return index_mask;
}

static void drmmode_clones_init(ScrnInfoPtr scrn, drmmode_ptr drmmode)
{
	int i, j;
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn);

	for (i = 0; i < xf86_config->num_output; i++) {
		xf86OutputPtr output = xf86_config->output[i];
		drmmode_output_private_ptr drmmode_output;

		drmmode_output = output->driver_private;
		drmmode_output->enc_clone_mask = 0xff;
		/* and all the possible encoder clones for this output together */
		for (j = 0; j < drmmode_output->mode_output->count_encoders;
		     j++) {
			int k;
			for (k = 0; k < drmmode->mode_res->count_encoders; k++) {
				if (drmmode->mode_res->encoders[k] ==
				    drmmode_output->
				    mode_encoders[j]->encoder_id)
					drmmode_output->enc_mask |= (1 << k);
			}

			drmmode_output->enc_clone_mask &=
			    drmmode_output->mode_encoders[j]->possible_clones;
		}
	}

	for (i = 0; i < xf86_config->num_output; i++) {
		xf86OutputPtr output = xf86_config->output[i];
		output->possible_clones = find_clones(scrn, output);
	}
}

/* returns pitch alignment in pixels */
int drmmode_get_pitch_align(ScrnInfoPtr scrn, int bpe)
{
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);

	if (info->have_tiling_info)
		/* linear aligned requirements */
		return MAX(64, info->group_bytes / bpe);
	else
		/* default to 512 elements if we don't know the real
		 * group size otherwise the kernel may reject the CS
		 * if the group sizes don't match as the pitch won't
		 * be aligned properly.
		 */
		return 512;
}

static Bool drmmode_xf86crtc_resize(ScrnInfoPtr scrn, int width, int height)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn);
	drmmode_crtc_private_ptr
	    drmmode_crtc = xf86_config->crtc[0]->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	AMDGPUInfoPtr info = AMDGPUPTR(scrn);
	struct amdgpu_buffer *old_front = NULL;
	ScreenPtr screen = xf86ScrnToScreen(scrn);
	uint32_t old_fb_id;
	int i, pitch, old_width, old_height, old_pitch;
	int cpp = info->pixel_bytes;
	PixmapPtr ppix = screen->GetScreenPixmap(screen);
	uint32_t bo_handle;
	void *fb_shadow;
	int hint = 0;

	if (scrn->virtualX == width && scrn->virtualY == height)
		return TRUE;

	if (info->shadow_primary)
		hint = AMDGPU_CREATE_PIXMAP_LINEAR | AMDGPU_CREATE_PIXMAP_GTT;
	else if (!info->use_glamor)
		hint = AMDGPU_CREATE_PIXMAP_LINEAR;

	xf86DrvMsg(scrn->scrnIndex, X_INFO,
		   "Allocate new frame buffer %dx%d\n", width, height);

	old_width = scrn->virtualX;
	old_height = scrn->virtualY;
	old_pitch = scrn->displayWidth;
	old_fb_id = drmmode->fb_id;
	old_front = info->front_buffer;

	scrn->virtualX = width;
	scrn->virtualY = height;

	info->front_buffer =
		amdgpu_alloc_pixmap_bo(scrn, scrn->virtualX, scrn->virtualY,
				       scrn->depth, hint, scrn->bitsPerPixel,
				       &pitch);
	if (!info->front_buffer) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
			   "Failed to allocate front buffer memory\n");
		goto fail;
	}

	if (amdgpu_bo_map(scrn, info->front_buffer)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
			   "Failed to map front buffer memory\n");
		goto fail;
	}

	xf86DrvMsg(scrn->scrnIndex, X_INFO, " => pitch %d bytes\n", pitch);
	scrn->displayWidth = pitch / cpp;

	if (!amdgpu_bo_get_handle(info->front_buffer, &bo_handle)) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
			   "Failed to get front buffer handle\n");
		goto fail;
	}

	if (drmModeAddFB(drmmode->fd, width, height, scrn->depth,
			 scrn->bitsPerPixel, pitch,
			 bo_handle, &drmmode->fb_id) != 0) {
		xf86DrvMsg(scrn->scrnIndex, X_ERROR,
			   "drmModeAddFB failed for front buffer\n");
		goto fail;
	}

	if (info->use_glamor ||
	    (info->front_buffer->flags & AMDGPU_BO_FLAGS_GBM)) {
		amdgpu_set_pixmap_bo(ppix, info->front_buffer);
		screen->ModifyPixmapHeader(ppix,
					   width, height, -1, -1, pitch, info->front_buffer->cpu_ptr);
	} else {
		fb_shadow = calloc(1, pitch * scrn->virtualY);
		if (fb_shadow == NULL)
			goto fail;
		free(info->fb_shadow);
		info->fb_shadow = fb_shadow;
		screen->ModifyPixmapHeader(ppix,
					   width, height, -1, -1, pitch,
					   info->fb_shadow);
	}
#if XORG_VERSION_CURRENT < XORG_VERSION_NUMERIC(1,9,99,1,0)
	scrn->pixmapPrivate.ptr = ppix->devPrivate.ptr;
#endif

	for (i = 0; i < xf86_config->num_crtc; i++) {
		xf86CrtcPtr crtc = xf86_config->crtc[i];

		if (!crtc->enabled)
			continue;

		drmmode_set_mode_major(crtc, &crtc->mode,
				       crtc->rotation, crtc->x, crtc->y);
	}

	if (info->use_glamor)
		amdgpu_glamor_create_screen_resources(scrn->pScreen);

	if (old_fb_id)
		drmModeRmFB(drmmode->fd, old_fb_id);
	if (old_front) {
		amdgpu_bo_unref(&old_front);
	}

	return TRUE;

fail:
	if (info->front_buffer) {
		amdgpu_bo_unref(&info->front_buffer);
	}
	info->front_buffer = old_front;
	scrn->virtualX = old_width;
	scrn->virtualY = old_height;
	scrn->displayWidth = old_pitch;
	drmmode->fb_id = old_fb_id;

	return FALSE;
}

static const xf86CrtcConfigFuncsRec drmmode_xf86crtc_config_funcs = {
	drmmode_xf86crtc_resize
};

static void
drmmode_flip_free(drmmode_flipevtcarrier_ptr flipcarrier)
{
	drmmode_flipdata_ptr flipdata = flipcarrier->flipdata;

	free(flipcarrier);

	if (--flipdata->flip_count > 0)
		return;

	/* Release framebuffer */
	drmModeRmFB(flipdata->drmmode->fd, flipdata->old_fb_id);

	free(flipdata);
}

static void
drmmode_flip_abort(ScrnInfoPtr scrn, void *event_data)
{
	drmmode_flipevtcarrier_ptr flipcarrier = event_data;
	drmmode_flipdata_ptr flipdata = flipcarrier->flipdata;

	if (flipdata->flip_count == 1)
		flipcarrier->abort(scrn, flipdata->event_data);

	drmmode_flip_free(flipcarrier);
}

static void
drmmode_flip_handler(ScrnInfoPtr scrn, uint32_t frame, uint64_t usec, void *event_data)
{
	drmmode_flipevtcarrier_ptr flipcarrier = event_data;
	drmmode_flipdata_ptr flipdata = flipcarrier->flipdata;

	/* Is this the event whose info shall be delivered to higher level? */
	if (flipcarrier->dispatch_me) {
		/* Yes: Cache msc, ust for later delivery. */
		flipdata->fe_frame = frame;
		flipdata->fe_usec = usec;
	}

	/* Deliver cached msc, ust from reference crtc to flip event handler */
	if (flipdata->event_data && flipdata->flip_count == 1)
		flipcarrier->handler(scrn, flipdata->fe_frame, flipdata->fe_usec,
				     flipdata->event_data);

	drmmode_flip_free(flipcarrier);
}

static void drm_wakeup_handler(pointer data, int err, pointer p)
{
	drmmode_ptr drmmode = data;
	fd_set *read_mask = p;

	if (err >= 0 && FD_ISSET(drmmode->fd, read_mask)) {
		drmHandleEvent(drmmode->fd, &drmmode->event_context);
	}
}

Bool drmmode_pre_init(ScrnInfoPtr pScrn, drmmode_ptr drmmode, int cpp)
{
	int i;

	xf86CrtcConfigInit(pScrn, &drmmode_xf86crtc_config_funcs);

	drmmode->scrn = pScrn;
	drmmode->cpp = cpp;
	drmmode->mode_res = drmModeGetResources(drmmode->fd);
	if (!drmmode->mode_res)
		return FALSE;

	xf86CrtcSetSizeRange(pScrn, 320, 200, drmmode->mode_res->max_width,
			     drmmode->mode_res->max_height);
	for (i = 0; i < drmmode->mode_res->count_crtcs; i++)
		if (!xf86IsEntityShared(pScrn->entityList[0])
		    || pScrn->confScreen->device->screen == i)
			drmmode_crtc_init(pScrn, drmmode, i);

	for (i = 0; i < drmmode->mode_res->count_connectors; i++)
		drmmode_output_init(pScrn, drmmode, i);

	/* workout clones */
	drmmode_clones_init(pScrn, drmmode);

#ifdef AMDGPU_PIXMAP_SHARING
	xf86ProviderSetup(pScrn, NULL, "amdgpu");
#endif

	xf86InitialConfiguration(pScrn, TRUE);

	drmmode->event_context.version = DRM_EVENT_CONTEXT_VERSION;
	drmmode->event_context.vblank_handler = amdgpu_drm_queue_handler;
	drmmode->event_context.page_flip_handler = amdgpu_drm_queue_handler;

	return TRUE;
}

void drmmode_init(ScrnInfoPtr pScrn, drmmode_ptr drmmode)
{
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);

	info->drmmode_inited = TRUE;
	if (pAMDGPUEnt->fd_wakeup_registered != serverGeneration) {
		AddGeneralSocket(drmmode->fd);
		RegisterBlockAndWakeupHandlers((BlockHandlerProcPtr) NoopDDA,
					       drm_wakeup_handler, drmmode);
		pAMDGPUEnt->fd_wakeup_registered = serverGeneration;
		pAMDGPUEnt->fd_wakeup_ref = 1;
	} else
		pAMDGPUEnt->fd_wakeup_ref++;
}

void drmmode_fini(ScrnInfoPtr pScrn, drmmode_ptr drmmode)
{
	AMDGPUEntPtr pAMDGPUEnt = AMDGPUEntPriv(pScrn);
	AMDGPUInfoPtr info = AMDGPUPTR(pScrn);

	if (!info->drmmode_inited)
		return;

	if (pAMDGPUEnt->fd_wakeup_registered == serverGeneration &&
	    !--pAMDGPUEnt->fd_wakeup_ref) {
		RemoveGeneralSocket(drmmode->fd);
		RemoveBlockAndWakeupHandlers((BlockHandlerProcPtr) NoopDDA,
					     drm_wakeup_handler, drmmode);
	}
}

void drmmode_set_cursor(ScrnInfoPtr scrn, drmmode_ptr drmmode, int id,
			struct amdgpu_buffer *bo)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(scrn);
	xf86CrtcPtr crtc = xf86_config->crtc[id];
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;

	drmmode_crtc->cursor_buffer = bo;
}

void drmmode_adjust_frame(ScrnInfoPtr pScrn, drmmode_ptr drmmode, int x, int y)
{
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(pScrn);
	xf86OutputPtr output = config->output[config->compat_output];
	xf86CrtcPtr crtc = output->crtc;

	if (crtc && crtc->enabled) {
		drmmode_set_mode_major(crtc, &crtc->mode, crtc->rotation, x, y);
	}
}

Bool drmmode_set_desired_modes(ScrnInfoPtr pScrn, drmmode_ptr drmmode)
{
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(pScrn);
	int c;

	drmmode_copy_fb(pScrn, drmmode);

	for (c = 0; c < config->num_crtc; c++) {
		xf86CrtcPtr crtc = config->crtc[c];
		drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
		xf86OutputPtr output = NULL;
		int o;

		/* Skip disabled CRTCs */
		if (!crtc->enabled) {
			drmModeSetCrtc(drmmode->fd,
				       drmmode_crtc->mode_crtc->crtc_id, 0, 0,
				       0, NULL, 0, NULL);
			continue;
		}

		if (config->output[config->compat_output]->crtc == crtc)
			output = config->output[config->compat_output];
		else {
			for (o = 0; o < config->num_output; o++)
				if (config->output[o]->crtc == crtc) {
					output = config->output[o];
					break;
				}
		}
		/* paranoia */
		if (!output)
			continue;

		/* Mark that we'll need to re-set the mode for sure */
		memset(&crtc->mode, 0, sizeof(crtc->mode));
		if (!crtc->desiredMode.CrtcHDisplay) {
			DisplayModePtr mode = xf86OutputFindClosestMode(output,
									pScrn->
									currentMode);

			if (!mode)
				return FALSE;
			crtc->desiredMode = *mode;
			crtc->desiredRotation = RR_Rotate_0;
			crtc->desiredX = 0;
			crtc->desiredY = 0;
		}

		if (!crtc->funcs->set_mode_major(crtc, &crtc->desiredMode,
						 crtc->desiredRotation,
						 crtc->desiredX,
						 crtc->desiredY))
			return FALSE;
	}
	return TRUE;
}

static void drmmode_load_palette(ScrnInfoPtr pScrn, int numColors,
				 int *indices, LOCO * colors, VisualPtr pVisual)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
	uint16_t lut_r[256], lut_g[256], lut_b[256];
	int index, j, i;
	int c;

	for (c = 0; c < xf86_config->num_crtc; c++) {
		xf86CrtcPtr crtc = xf86_config->crtc[c];
		drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;

		for (i = 0; i < 256; i++) {
			lut_r[i] = drmmode_crtc->lut_r[i] << 6;
			lut_g[i] = drmmode_crtc->lut_g[i] << 6;
			lut_b[i] = drmmode_crtc->lut_b[i] << 6;
		}

		switch (pScrn->depth) {
		case 15:
			for (i = 0; i < numColors; i++) {
				index = indices[i];
				for (j = 0; j < 8; j++) {
					lut_r[index * 8 + j] =
					    colors[index].red << 6;
					lut_g[index * 8 + j] =
					    colors[index].green << 6;
					lut_b[index * 8 + j] =
					    colors[index].blue << 6;
				}
			}
			break;
		case 16:
			for (i = 0; i < numColors; i++) {
				index = indices[i];

				if (i <= 31) {
					for (j = 0; j < 8; j++) {
						lut_r[index * 8 + j] =
						    colors[index].red << 6;
						lut_b[index * 8 + j] =
						    colors[index].blue << 6;
					}
				}

				for (j = 0; j < 4; j++) {
					lut_g[index * 4 + j] =
					    colors[index].green << 6;
				}
			}
			break;
		default:
			for (i = 0; i < numColors; i++) {
				index = indices[i];
				lut_r[index] = colors[index].red << 6;
				lut_g[index] = colors[index].green << 6;
				lut_b[index] = colors[index].blue << 6;
			}
			break;
		}

		/* Make the change through RandR */
		if (crtc->randr_crtc)
			RRCrtcGammaSet(crtc->randr_crtc, lut_r, lut_g, lut_b);
		else
			crtc->funcs->gamma_set(crtc, lut_r, lut_g, lut_b, 256);
	}
}

Bool drmmode_setup_colormap(ScreenPtr pScreen, ScrnInfoPtr pScrn)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);

	if (xf86_config->num_crtc) {
		xf86DrvMsgVerb(pScrn->scrnIndex, X_INFO, AMDGPU_LOGLEVEL_DEBUG,
			       "Initializing kms color map\n");
		if (!miCreateDefColormap(pScreen))
			return FALSE;
		/* all amdgpus support 10 bit CLUTs */
		if (!xf86HandleColormaps(pScreen, 256, 10,
					 drmmode_load_palette, NULL,
					 CMAP_PALETTED_TRUECOLOR
#if 0				/* This option messes up text mode! (eich@suse.de) */
					 | CMAP_LOAD_EVEN_IF_OFFSCREEN
#endif
					 | CMAP_RELOAD_ON_MODE_SWITCH))
			return FALSE;
	}
	return TRUE;
}

#ifdef HAVE_LIBUDEV
static void drmmode_handle_uevents(int fd, void *closure)
{
	drmmode_ptr drmmode = closure;
	ScrnInfoPtr scrn = drmmode->scrn;
	struct udev_device *dev;
	dev = udev_monitor_receive_device(drmmode->uevent_monitor);
	if (!dev)
		return;

	RRGetInfo(xf86ScrnToScreen(scrn), TRUE);
	udev_device_unref(dev);
}
#endif

void drmmode_uevent_init(ScrnInfoPtr scrn, drmmode_ptr drmmode)
{
#ifdef HAVE_LIBUDEV
	struct udev *u;
	struct udev_monitor *mon;

	u = udev_new();
	if (!u)
		return;
	mon = udev_monitor_new_from_netlink(u, "udev");
	if (!mon) {
		udev_unref(u);
		return;
	}

	if (udev_monitor_filter_add_match_subsystem_devtype(mon,
							    "drm",
							    "drm_minor") < 0 ||
	    udev_monitor_enable_receiving(mon) < 0) {
		udev_monitor_unref(mon);
		udev_unref(u);
		return;
	}

	drmmode->uevent_handler =
	    xf86AddGeneralHandler(udev_monitor_get_fd(mon),
				  drmmode_handle_uevents, drmmode);

	drmmode->uevent_monitor = mon;
#endif
}

void drmmode_uevent_fini(ScrnInfoPtr scrn, drmmode_ptr drmmode)
{
#ifdef HAVE_LIBUDEV
	if (drmmode->uevent_handler) {
		struct udev *u = udev_monitor_get_udev(drmmode->uevent_monitor);
		xf86RemoveGeneralHandler(drmmode->uevent_handler);

		udev_monitor_unref(drmmode->uevent_monitor);
		udev_unref(u);
	}
#endif
}

Bool amdgpu_do_pageflip(ScrnInfoPtr scrn, ClientPtr client,
			struct amdgpu_buffer *new_front, uint64_t id, void *data,
			int ref_crtc_hw_id, amdgpu_drm_handler_proc handler,
			amdgpu_drm_abort_proc abort)
{
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(scrn);
	drmmode_crtc_private_ptr drmmode_crtc = config->crtc[0]->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	unsigned int pitch;
	int i, old_fb_id;
	int height, emitted = 0;
	drmmode_flipdata_ptr flipdata;
	drmmode_flipevtcarrier_ptr flipcarrier = NULL;
	struct amdgpu_drm_queue_entry *drm_queue = 0;
	union gbm_bo_handle bo_handle;
	uint32_t handle;

	if (new_front->flags & AMDGPU_BO_FLAGS_GBM) {
		pitch = gbm_bo_get_stride(new_front->bo.gbm);
		height = gbm_bo_get_height(new_front->bo.gbm);
		bo_handle = gbm_bo_get_handle(new_front->bo.gbm);
		handle = bo_handle.u32;
	} else {
		pitch = scrn->displayWidth;
		height = scrn->virtualY;
		if (amdgpu_bo_export(new_front->bo.amdgpu,
				amdgpu_bo_handle_type_kms,
				&handle))
			goto error_out;
	}

	/*
	 * Create a new handle for the back buffer
	 */
	old_fb_id = drmmode->fb_id;

	if (drmModeAddFB(drmmode->fd, scrn->virtualX, height,
			 scrn->depth, scrn->bitsPerPixel, pitch,
			 handle, &drmmode->fb_id)) {
		goto error_out;
	}
	flipdata = calloc(1, sizeof(drmmode_flipdata_rec));
	if (!flipdata) {
		xf86DrvMsg(scrn->scrnIndex, X_WARNING,
			   "flip queue: data alloc failed.\n");
		goto error_undo;
	}
	/*
	 * Queue flips on all enabled CRTCs
	 * Note that if/when we get per-CRTC buffers, we'll have to update this.
	 * Right now it assumes a single shared fb across all CRTCs, with the
	 * kernel fixing up the offset of each CRTC as necessary.
	 *
	 * Also, flips queued on disabled or incorrectly configured displays
	 * may never complete; this is a configuration error.
	 */

	flipdata->event_data = data;
	flipdata->drmmode = drmmode;

	for (i = 0; i < config->num_crtc; i++) {
		if (!config->crtc[i]->enabled)
			continue;

		flipdata->flip_count++;
		drmmode_crtc = config->crtc[i]->driver_private;

		flipcarrier = calloc(1, sizeof(drmmode_flipevtcarrier_rec));
		if (!flipcarrier) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "flip queue: carrier alloc failed.\n");
			if (emitted == 0)
				free(flipdata);
			goto error_undo;
		}

		/* Only the reference crtc will finally deliver its page flip
		 * completion event. All other crtc's events will be discarded.
		 */
		flipcarrier->dispatch_me =
		    (drmmode_crtc->hw_id == ref_crtc_hw_id);
		flipcarrier->flipdata = flipdata;
		flipcarrier->handler = handler;
		flipcarrier->abort = abort;

		drm_queue = amdgpu_drm_queue_alloc(scrn, client, id,
						   flipcarrier,
						   drmmode_flip_handler,
						   drmmode_flip_abort);
		if (!drm_queue) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "Allocating DRM queue event entry failed.\n");
			goto error_undo;
		}

		if (drmModePageFlip(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id,
				    drmmode->fb_id, DRM_MODE_PAGE_FLIP_EVENT,
				    drm_queue)) {
			xf86DrvMsg(scrn->scrnIndex, X_WARNING,
				   "flip queue failed: %s\n", strerror(errno));
			free(flipcarrier);
			if (emitted == 0)
				free(flipdata);
			goto error_undo;
		}
		emitted++;
	}

	flipdata->old_fb_id = old_fb_id;
	return TRUE;

error_undo:
	if (drm_queue)
		amdgpu_drm_abort_entry(drm_queue);
	else
		drmmode_flip_abort(scrn, flipcarrier);

	drmModeRmFB(drmmode->fd, drmmode->fb_id);
	drmmode->fb_id = old_fb_id;

error_out:
	xf86DrvMsg(scrn->scrnIndex, X_WARNING, "Page flip failed: %s\n",
		   strerror(errno));
	return FALSE;
}
