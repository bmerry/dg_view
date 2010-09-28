/*
   This file is part of Datagrind, a tool for tracking data accesses.

   Copyright (C) 2010 Bruce Merry
      bmerry@users.sourceforge.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
*/

#include <gtk/gtk.h>
#include <glib.h>
#include <gdk/gdk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <cstdlib>
#include <cmath>
#include <algorithm>

#include "dg_view.h"
#include "dg_view_debuginfo.h"
#include "dg_record.h"

using namespace std;

struct viewer_region
{
    GtkWidget *image;   /* GtkImage */
    GtkWidget *events;  /* GtkEventBox */
    GdkPixbuf *pixbuf;

    double addr_min, addr_max;
    double iseq_min, iseq_max;

    int click_x;
    int click_y;
    bool in_click;
};

struct viewer
{
    GtkWidget *window; /* GtkWindow */
    viewer_region region;
};

static gboolean on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data);
static gboolean on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data);

static void build_region(viewer_region &vr, int width, int height)
{
    vr.pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
    if (vr.pixbuf == NULL)
    {
        fprintf(stderr, "Could not allocate a %d x %d pixbuf\n", width, height);
        exit(1);
    }

    vr.image = gtk_image_new_from_pixbuf(vr.pixbuf);
    if (vr.image == NULL)
    {
        fprintf(stderr, "Could not allocate an image for the pixbuf\n");
        exit(1);
    }

    vr.events = gtk_event_box_new();
    if (vr.events == NULL)
    {
        fprintf(stderr, "Could not allocate an event box\n");
        exit(1);
    }
    gtk_widget_add_events(vr.events, GDK_BUTTON_PRESS_MASK | GDK_BUTTON_RELEASE_MASK);
    g_signal_connect(G_OBJECT(vr.events), "button-press-event",
                     G_CALLBACK(on_press), &vr);
    g_signal_connect(G_OBJECT(vr.events), "button-release-event",
                     G_CALLBACK(on_release), &vr);
    gtk_container_add(GTK_CONTAINER(vr.events), vr.image);

    gtk_widget_set_size_request(vr.image, width, height);

    gdk_pixbuf_fill(vr.pixbuf, 0x00000000);
}

static void build_main_window(viewer &v)
{
    v.window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(v.window), "dg_view");
    g_signal_connect(G_OBJECT(v.window), "destroy",
                     G_CALLBACK(gtk_main_quit), NULL);

    build_region(v.region, 800, 800);
    gtk_container_add(GTK_CONTAINER(v.window), v.region.events);

    gtk_widget_show_all(v.window);
}

static void prepare_min_max(viewer_region &vr)
{
    const bbrun_list &bbruns = dg_view_bbruns();
    const page_map &pages = dg_view_page_map();
    vr.addr_min = pages.begin()->second;
    vr.addr_max = (--pages.end())->second + DG_VIEW_PAGE_SIZE;
    vr.iseq_min = 0;

    bbrun_list::const_iterator last_bbrun = --bbruns.end();
    const bbdef &last_bbdef = dg_view_bbrun_get_bbdef(*last_bbrun);
    vr.iseq_max = last_bbrun->iseq_start + last_bbdef.accesses.back().iseq + 1;
}

/* Takes a range in a semantic space and gives first and last pixels covered.
 * The return values are a [first, last) range. Returns false if there is
 * no intersection with the viewport.
 */
static bool range_to_pixels(double lo, double hi,
                            double view_min, double view_max, int pixels,
                            int &pixel_lo, int &pixel_hi)
{
    if (hi <= view_min || lo >= view_max)
        return false;
    double scale = pixels / (view_max - view_min);
    pixel_lo = (int) floor((lo - view_min) * scale + 0.5);
    pixel_hi = (int) floor((hi - view_min) * scale + 0.f);
    if (pixel_hi == pixel_lo)
        pixel_hi++;
    pixel_lo = max(pixel_lo, 0);
    pixel_hi = min(pixel_hi, pixels);
    return true;
}

static void update_region(viewer_region &vr)
{
    int rowstride = gdk_pixbuf_get_rowstride(vr.pixbuf);
    guchar *pixels = gdk_pixbuf_get_pixels(vr.pixbuf);
    int width = gdk_pixbuf_get_width(vr.pixbuf);
    int height = gdk_pixbuf_get_height(vr.pixbuf);
    int n_channels = gdk_pixbuf_get_n_channels(vr.pixbuf);

    g_return_if_fail(n_channels == 3);
    g_return_if_fail(gdk_pixbuf_get_colorspace(vr.pixbuf) == GDK_COLORSPACE_RGB);
    g_return_if_fail(gdk_pixbuf_get_bits_per_sample(vr.pixbuf) == 8);
    gdk_pixbuf_fill(vr.pixbuf, 0);

    const bbrun_list &bbruns = dg_view_bbruns();
    for (bbrun_list::const_iterator bbr = bbruns.begin(); bbr != bbruns.end(); ++bbr)
    {
        for (size_t j = 0; j < bbr->n_addrs; j++)
            if (bbr->addrs[j])
            {
                const bbdef &bbd = dg_view_bbrun_get_bbdef(*bbr);
                g_assert(j < bbd.accesses.size());
                const bbdef_access &bbda = bbd.accesses[j];

                size_t addr = dg_view_remap_address(bbr->addrs[j]);
                size_t iseq = bbr->iseq_start + bbda.iseq;
                int x1, x2, y1, y2;

                if (range_to_pixels(addr, addr + bbda.size,
                                    vr.addr_min, vr.addr_max, width,
                                    x1, x2) &&
                    range_to_pixels(iseq, iseq + 1,
                                    vr.iseq_min, vr.iseq_max, height,
                                    y1, y2))
                {
                    for (int y = y1; y < y2; y++)
                        for (int x = x1; x < x2; x++)
                        {
                            guchar * const p = pixels + y * rowstride + x * n_channels;
                            switch (bbda.dir)
                            {
                            case DG_ACC_READ:
                                p[1] = 255; /* green */
                                break;
                            case DG_ACC_WRITE:
                                p[0] = 255; /* red */
                                break;
                            default:
                                g_assert_not_reached();
                            }
                        }
                }
            }
    }

    gtk_widget_queue_draw(vr.image);
}

static gboolean on_press(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    viewer_region *vr = (viewer_region *) user_data;

    vr->in_click = true;
    vr->click_x = event->x;
    vr->click_y = event->y;
    gtk_grab_add(widget);
    return FALSE;
}

/* Computes new values for amin and amax (which are in an abstract space,
 * based on two coordinates of a zoom box in a window space [0, size).
 */
static void update_zoom(double *amin, double *amax, int w1, int w2, int size)
{
    /* Convert to pixel centers */
    double l = min(w1, w2) + 0.5;
    double h = max(w1, w2) + 0.5;

    /* Convert to parametric coordinates in [0, 1] */
    l /= size;
    h /= size;

    double scale = h - l;
    /* Expand slightly so that some context is visible */
    double expand = 0.1f * scale;
    l -= expand;
    h += expand;

    /* Clamp to the window */
    l = max(l, 0.0);
    h = min(h, 1.0);

    /* Interpolate to get new coordinates */
    double old_size = *amax - *amin;
    *amax = *amin + h * old_size;
    *amin = *amin + l * old_size;
}

static gboolean on_release(GtkWidget *widget, GdkEventButton *event, gpointer user_data)
{
    gtk_grab_remove(widget);

    viewer_region *vr = (viewer_region *) user_data;
    if (vr->in_click)
    {
        vr->in_click = false;
        int width = gdk_pixbuf_get_width(vr->pixbuf);
        int height = gdk_pixbuf_get_height(vr->pixbuf);
        if (abs(event->x - vr->click_x) > 2 && abs(event->y - vr->click_y) > 2)
        {
            /* Assume it was a drag to zoom */
            update_zoom(&vr->addr_min, &vr->addr_max,
                        vr->click_x, event->x, width);
            update_zoom(&vr->iseq_min, &vr->iseq_max,
                        vr->click_y, event->y, height);
            update_region(*vr);
        }
        else
        {
            /* A click to get information */
            double addr_size = vr->addr_max - vr->addr_min;
            double iseq_size = vr->iseq_max - vr->iseq_min;

            double addr_scale = addr_size / width;
            double iseq_scale = iseq_size / height;
            double ratio = iseq_scale / addr_scale;

            HWord remapped = (HWord) (0.5 + (event->x + 0.5) * addr_scale + vr->addr_min);
            HWord addr = dg_view_revmap_addr(remapped);
            double iseq = (event->y + 0.5) * iseq_scale + vr->iseq_min;

            mem_access access = dg_view_nearest_access(addr, iseq, ratio);
            if (access.size != 0)
            {
                printf("Nearest access: %#zx", access.addr);
                mem_block *block = access.block;
                if (block != NULL)
                {
                    printf(": %zu bytes inside a block of size %zu, allocated at\n",
                           access.addr - block->addr, block->size);
                    for (size_t i = 0; i < block->stack.size(); i++)
                    {
                        string loc = dg_view_addr2line(block->stack[i]);
                        printf("  %s\n", loc.c_str());
                    }
                }
                else
                    printf("\n");

                if (!access.stack.empty())
                {
                    printf("At\n");
                    for (size_t i = 0; i < access.stack.size();i++)
                    {
                        string loc = dg_view_addr2line(access.stack[i]);
                        printf("  %s\n", loc.c_str());
                    }
                }
            }
        }
    }
    return FALSE;
}

int main(int argc, char **argv)
{
    viewer main_viewer;
    g_thread_init(NULL);
    gtk_init(&argc, &argv);

    dg_view_parse_opts(&argc, argv);
    if (argc != 2)
    {
        dg_view_usage(argv[0], 2);
    }
    if (!dg_view_load(argv[1]))
        return 1;

    build_main_window(main_viewer);
    prepare_min_max(main_viewer.region);
    update_region(main_viewer.region);
    gtk_main();
    return 0;
}
