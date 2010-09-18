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

#include <GL/glew.h>
#include <GL/glut.h>
#include <cmath>
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "dg_view.h"
#include "dg_view_debuginfo.h"
#include "dg_record.h"

using namespace std;

#define DG_VIEW_SINGLE_DOT 1 /* Set to 1 to show only one dot per multi-byte access */

static GLuint num_vertices;

typedef struct
{
    GLfloat pos[2];
    GLubyte color[4];
} vertex;

static GLfloat min_x, max_x, min_y, max_y;
static GLfloat window_width, window_height;
static GLint zoom_x, zoom_y;

static size_t count_access_bytes(void)
{
    size_t total = 0;
    const bbrun_list &bbruns = dg_view_bbruns();
    for (bbrun_list::const_iterator bbr = bbruns.begin(); bbr != bbruns.end(); ++bbr)
    {
        for (size_t j = 0; j < bbr->n_addrs; j++)
            if (bbr->addrs[j])
            {
                const bbdef &bbd = dg_view_bbrun_get_bbdef(*bbr);
                assert(j < bbd.accesses.size());
                const bbdef_access &bbda = bbd.accesses[j];
#if DG_VIEW_SINGLE_DOT
                total++;
#else
                total += bbda.size;
#endif
            }
    }
    return total;
}

static void init_gl(void)
{
    GLuint vbo;
    GLubyte color_read[4] = {0, 255, 0, 255};
    GLubyte color_write[4] = {0, 0, 255, 255};
    GLubyte color_instr[4] = {255, 0, 0, 255};
    vertex *start = NULL;
    num_vertices = count_access_bytes();

    vector<vertex> vertices(num_vertices);
    min_x = HUGE_VALF;
    max_x = -HUGE_VALF;

    size_t v = 0;
    const bbrun_list &bbruns = dg_view_bbruns();
    for (bbrun_list::const_iterator bbr = bbruns.begin(); bbr != bbruns.end(); ++bbr)
    {
        for (size_t j = 0; j < bbr->n_addrs; j++)
            if (bbr->addrs[j])
            {
                const bbdef &bbd = dg_view_bbrun_get_bbdef(*bbr);
                assert(j < bbd.accesses.size());
                const bbdef_access &bbda = bbd.accesses[j];
#if DG_VIEW_SINGLE_DOT
                int dots = 1;
#else
                int dots = bbda.size;
#endif
                for (int k = 0; k < dots; k++)
                {
                    vertices[v].pos[0] = dg_view_remap_address(bbr->addrs[j]) + k;
                    vertices[v].pos[1] = bbr->iseq_start + bbda.iseq;
                    min_x = min(min_x, vertices[v].pos[0]);
                    max_x = max(max_x, vertices[v].pos[0]);
                    switch (bbda.dir)
                    {
                    case DG_ACC_READ:
                        memcpy(vertices[v].color, color_read, sizeof(color_read));
                        break;
                    case DG_ACC_WRITE:
                        memcpy(vertices[v].color, color_write, sizeof(color_write));
                        break;
                    case DG_ACC_EXEC:
                        memcpy(vertices[v].color, color_instr, sizeof(color_instr));
                        break;
                    }
                    v++;
                }
            }
    }
    assert(v == num_vertices);

    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    if (glGetError() != GL_NO_ERROR)
    {
        fprintf(stderr, "Error initialising GL state\n");
        exit(1);
    }
    glBufferData(GL_ARRAY_BUFFER, num_vertices * sizeof(vertex), &vertices[0], GL_STATIC_DRAW);
    if (glGetError() != GL_NO_ERROR)
    {
        fprintf(stderr,
                "Error loading buffer data. It may be more than your GL implementation can handle.\n"
                "Try using the --events and --ranges options.\n");
        exit(1);
    }

    glVertexPointer(2, GL_FLOAT, sizeof(vertex), &start->pos);
    glColorPointer(4, GL_UNSIGNED_BYTE, sizeof(vertex), &start->color);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glBlendFuncSeparate(GL_ONE, GL_DST_ALPHA, GL_ONE, GL_ZERO);
    glEnable(GL_BLEND);

    glBindBuffer(GL_ARRAY_BUFFER, 0);

    min_y = vertices[0].pos[1] - 1.0f;
    max_y = vertices.back().pos[1] + 1.0f;
    min_x -= 0.5f;
    max_x += 0.5f;

    if (glGetError() != GL_NO_ERROR)
    {
        fprintf(stderr, "Error initialising GL state\n");
        exit(1);
    }
}

static void display(void)
{
    const page_map &pm = dg_view_page_map();

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT | GL_STENCIL_BUFFER_BIT);

    glLoadIdentity();
    glOrtho(min_x, max_x, max_y, min_y, -1.0, 1.0);

    HWord last = 0;
    GLfloat xrate = (max_x - min_x) / window_width;
    glBegin(GL_LINES);
    for (page_map::const_iterator i = pm.begin(); i != pm.end(); ++i)
    {
        if (i->first != last + DG_VIEW_PAGE_SIZE)
        {
            glColor4ub(192, 192, 192, 0);
            glVertex2f(i->second, min_y);
            glVertex2f(i->second, max_y);
        }
        else if (xrate < DG_VIEW_PAGE_SIZE / 8)
        {
            glColor4ub(64, 64, 64, 0);
            glVertex2f(i->second, min_y);
            glVertex2f(i->second, max_y);
        }
        if (xrate < DG_VIEW_LINE_SIZE / 8)
        {
            glColor4ub(96, 32, 32, 0);
            for (int j = DG_VIEW_LINE_SIZE; j < DG_VIEW_PAGE_SIZE; j += DG_VIEW_LINE_SIZE)
            {
                glVertex2f(i->second + j, min_y);
                glVertex2f(i->second + j, max_y);
            }
        }
        last = i->first;
    }
    glEnd();

    glDrawArrays(GL_POINTS, 0, num_vertices);

    glutSwapBuffers();
}

static void mouse(int button, int state, int x, int y)
{
    if (button == GLUT_LEFT_BUTTON)
    {
        if (state == GLUT_DOWN)
        {
            zoom_x = x;
            zoom_y = y;
        }
        else if (abs(zoom_x - x) > 2 && abs(zoom_y - y) > 2)
        {
            GLfloat x1 = min_x + (zoom_x + 0.5) * (max_x - min_x) / window_width;
            GLfloat y1 = min_y + (zoom_y + 0.5) * (max_y - min_y) / window_height;
            GLfloat x2 = min_x + (x + 0.5) * (max_x - min_x) / window_width;
            GLfloat y2 = min_y + (y + 0.5) * (max_y - min_y) / window_height;

            min_x = min(x1, x2) - 0.5f;
            max_x = max(x1, x2) + 0.5f;
            min_y = min(y1, y2) - 0.5f;
            max_y = max(y1, y2) + 0.5f;
            glutPostRedisplay();
        }
        else
        {
            HWord remapped = (HWord) (0.5 + (GLdouble) (x + 0.5f) / window_width * (max_x - min_x) + min_x);
            HWord addr = dg_view_revmap_addr(remapped);
            double seq = (GLdouble) (y + 0.5f) / window_height * (max_y - min_y) + min_y;

            double addr_scale = window_width / (max_x - min_x);
            double seq_scale = window_height / (max_y - min_y);
            double ratio = addr_scale / seq_scale;

            mem_access access = dg_view_nearest_access(addr, seq, ratio);
            if (access.size != 0)
            {
                printf("Nearest access: %#zx", access.addr);
                mem_block *block = access.block;
                if (block != NULL)
                {
                    printf(": %zu bytes inside a block of size %zu, allocated at\n",
                           addr - block->addr, block->size);
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
}

static void reshape(int width, int height)
{
    window_width = width;
    window_height = height;
    glViewport(0, 0, width, height);
}

int main(int argc, char **argv)
{
    glutInit(&argc, argv);
    dg_view_parse_opts(&argc, argv);

    if (argc != 2)
    {
        dg_view_usage(argv[0], 2);
    }
    if (!dg_view_load(argv[1]))
        return 1;

    glutInitWindowSize(800, 800);
    glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);
    glutCreateWindow("dg_view");
    glutDisplayFunc(display);
    glutMouseFunc(mouse);
    glutReshapeFunc(reshape);
    glewInit();
    if (!GLEW_VERSION_1_5)
    {
        fprintf(stderr, "OpenGL 1.5 or later is required.\n");
        return 1;
    }
    init_gl();

    glutMainLoop();

    return 0;
}
