/*
 * Copyright © 2007 Red Hat, Inc.
 * Copyright © 2015 Advanced Micro Devices, Inc.
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

#include <xorg-server.h>

#include "amdgpu_drv.h"
#include "amdgpu_drm_queue.h"
#include "amdgpu_list.h"


struct amdgpu_drm_queue_entry {
	struct xorg_list list;
	uint64_t id;
	void *data;
	ClientPtr client;
	ScrnInfoPtr scrn;
	amdgpu_drm_handler_proc handler;
	amdgpu_drm_abort_proc abort;
};

static int amdgpu_drm_queue_refcnt;
static struct xorg_list amdgpu_drm_queue;


/*
 * Handle a DRM event
 */
void
amdgpu_drm_queue_handler(int fd, unsigned int frame, unsigned int sec,
			 unsigned int usec, void *user_ptr)
{
	struct amdgpu_drm_queue_entry *user_data = user_ptr;
	struct amdgpu_drm_queue_entry *e, *tmp;

	xorg_list_for_each_entry_safe(e, tmp, &amdgpu_drm_queue, list) {
		if (e == user_data) {
			xorg_list_del(&e->list);
			e->handler(e->scrn, frame,
				   (uint64_t)sec * 1000000 + usec, e->data);
			free(e);
			break;
		}
	}
}

/*
 * Enqueue a potential drm response; when the associated response
 * appears, we've got data to pass to the handler from here
 */
struct amdgpu_drm_queue_entry *
amdgpu_drm_queue_alloc(ScrnInfoPtr scrn, ClientPtr client,
		       uint64_t id, void *data,
		       amdgpu_drm_handler_proc handler,
		       amdgpu_drm_abort_proc abort)
{
	struct amdgpu_drm_queue_entry *e;

	e = calloc(1, sizeof(struct amdgpu_drm_queue_entry));
	if (!e)
		return NULL;

	e->client = client;
	e->scrn = scrn;
	e->id = id;
	e->data = data;
	e->handler = handler;
	e->abort = abort;

	xorg_list_add(&e->list, &amdgpu_drm_queue);

	return e;
}

/*
 * Abort one queued DRM entry, removing it
 * from the list, calling the abort function and
 * freeing the memory
 */
static void
amdgpu_drm_abort_one(struct amdgpu_drm_queue_entry *e)
{
	xorg_list_del(&e->list);
	e->abort(e->scrn, e->data);
	free(e);
}

/*
 * Abort drm queue entries for a client
 */
void
amdgpu_drm_abort_client(ClientPtr client)
{
	struct amdgpu_drm_queue_entry *e, *tmp;

	xorg_list_for_each_entry_safe(e, tmp, &amdgpu_drm_queue, list) {
		if (e->client == client)
			amdgpu_drm_abort_one(e);
	}
}

/*
 * Abort specific drm queue entry
 */
void
amdgpu_drm_abort_entry(struct amdgpu_drm_queue_entry *entry)
{
	amdgpu_drm_abort_one(entry);
}

/*
 * Abort specific drm queue entry by ID
 */
void
amdgpu_drm_abort_id(uint64_t id)
{
	struct amdgpu_drm_queue_entry *e, *tmp;

	xorg_list_for_each_entry_safe(e, tmp, &amdgpu_drm_queue, list) {
		if (e->id == id) {
			amdgpu_drm_abort_one(e);
			break;
		}
	}
}

/*
 * Initialize the DRM event queue
 */
void
amdgpu_drm_queue_init()
{
	if (amdgpu_drm_queue_refcnt++)
		return;

	xorg_list_init(&amdgpu_drm_queue);
}

/*
 * Deinitialize the DRM event queue
 */
void
amdgpu_drm_queue_close(ScrnInfoPtr scrn)
{
	struct amdgpu_drm_queue_entry *e, *tmp;

	xorg_list_for_each_entry_safe(e, tmp, &amdgpu_drm_queue, list) {
		if (e->scrn == scrn)
			amdgpu_drm_abort_one(e);
	}

	amdgpu_drm_queue_refcnt--;
}
