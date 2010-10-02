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

#ifndef DG_VIEW_H
#define DG_VIEW_H

#include <cstddef>
#include <stdint.h>
#include <vector>
#include <utility>
#include <string>
#include <map>

#define DG_VIEW_PAGE_SIZE 4096
#define DG_VIEW_LINE_SIZE 64

typedef uintptr_t HWord;

/* Memory block allocated with malloc or similar function in the guest */
struct mem_block
{
    HWord addr;
    HWord size;
    std::vector<HWord> stack;
    std::string label;
    bool matched;      /* Matches the requirements of -m, if any */
};

/* Not stored anywhere, only used to get information about accesses */
struct mem_access
{
    HWord addr;
    uint8_t dir;
    uint8_t size;
    mem_block *block;

    uint64_t iseq;
    std::vector<HWord> stack;

    mem_access() : addr(0), dir(0), size(0), block(NULL), iseq(0), stack() {}
};

struct context
{
    HWord bbdef_index;
    std::vector<HWord> stack;
};

struct bbdef_access
{
    uint8_t dir;
    uint8_t size;
    uint8_t iseq;
};

struct bbdef
{
    std::vector<HWord> instr_addrs;
    std::vector<bbdef_access> accesses;
};

struct bbrun
{
    uint64_t iseq_start;
    uint64_t dseq_start;
    uint32_t context_index;
    uint32_t n_addrs;
    HWord *addrs;                 /* Allocated from hword_pool; NULL means discarded */
    mem_block **blocks;           /* Allocated from mem_block_ptr_pool */
};

/* ratio is the ratio of address scale to iseq scale: a large value for ratio
 * increases the importance of the address in the match.
 *
 * Returns the best score and best index for the block. If there were no
 * usable addresses, returns score of HUGE_VAL;
 */
mem_access dg_view_nearest_access(double addr, double iseq, double ratio);

void dg_view_parse_opts(int *argc, char **argv);
void dg_view_usage(const char *prog, int code);
bool dg_view_load(const char *filename);

typedef std::map<HWord, size_t> page_map;
/* Takes a compressed address and maps it to an original one */
HWord dg_view_revmap_addr(std::size_t addr);
/* Maps original address to compressed address */
std::size_t dg_view_remap_address(HWord a);
const page_map &dg_view_page_map();

typedef std::vector<bbrun> bbrun_list;
const bbrun_list &dg_view_bbruns();
const bbdef &dg_view_bbrun_get_bbdef(const bbrun &bbr);

#endif /* DG_VIEW_H */
