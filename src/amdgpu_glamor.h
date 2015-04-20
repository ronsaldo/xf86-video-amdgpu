/*
 * Copyright © 2011 Intel Corporation.
 *             2012 Advanced Micro Devices, Inc.
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including
 * the next paragraph) shall be included in all copies or substantial
 * portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#ifndef AMDGPU_GLAMOR_H
#define AMDGPU_GLAMOR_H

#include "xf86xv.h"

Bool amdgpu_glamor_pre_init(ScrnInfoPtr scrn);
Bool amdgpu_glamor_init(ScreenPtr screen);
Bool amdgpu_glamor_create_screen_resources(ScreenPtr screen);
void amdgpu_glamor_free_screen(int scrnIndex, int flags);

void amdgpu_glamor_flush(ScrnInfoPtr pScrn);

Bool amdgpu_glamor_create_textured_pixmap(PixmapPtr pixmap);
void amdgpu_glamor_exchange_buffers(PixmapPtr src, PixmapPtr dst);

Bool amdgpu_glamor_pixmap_is_offscreen(PixmapPtr pixmap);

XF86VideoAdaptorPtr amdgpu_glamor_xv_init(ScreenPtr pScreen, int num_adapt);

#endif /* AMDGPU_GLAMOR_H */
