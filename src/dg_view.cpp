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

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cmath>
#include <cassert>
#include <stdint.h>
#include <getopt.h>
#include <vector>
#include <algorithm>
#include <set>
#include <map>
#include <string>
#include <memory>
#include <sstream>

#include "dg_record.h"
#include "dg_view.h"
#include "dg_view_range.h"
#include "dg_view_debuginfo.h"
#include "dg_view_parse.h"
#include "dg_view_pool.h"

using namespace std;

struct compare_bbrun_iseq
{
    bool operator()(const bbrun &a, const bbrun &b) const
    {
        return a.iseq_start < b.iseq_start;
    }

    bool operator()(const bbrun &a, uint64_t iseq) const
    {
        return a.iseq_start < iseq;
    }

    bool operator()(uint64_t iseq, const bbrun &a) const
    {
        return iseq < a.iseq_start;
    }
};

static pool_allocator<HWord> hword_pool;
static pool_allocator<mem_block *> mem_block_ptr_pool;

/* All START_EVENTs with no matching END_EVENT from chosen_events */
static multiset<string> active_events;
/* All TRACK_RANGEs with no matching UNTRACK_RANGE from chosen_ranges */
static multiset<pair<HWord, HWord> > active_ranges;
static bool malloc_only = false;

static rangemap<HWord, mem_block *> block_map;
static vector<mem_block *> block_storage;

/* Events selected on the command line, or empty if there wasn't a choice */
static set<string> chosen_events;
/* Ranges selected on the command line, or empty if there wasn't a choice */
static set<string> chosen_ranges;

static vector<bbdef> bbdefs;
static vector<bbrun> bbruns;
static vector<context> contexts;
static page_map fwd_page_map;
static map<size_t, HWord> rev_page_map;

template<typename T> T page_round_down(T x)
{
    return x & ~(DG_VIEW_PAGE_SIZE - 1);
}

/* ratio is the ratio of address scale to iseq scale: a large value for ratio
 * increases the importance of the address in the match.
 *
 * Returns the best score and best index for the block. If there were no
 * usable addresses, returns score of HUGE_VAL;
 */
static pair<double, size_t> nearest_access_bbrun(const bbrun &bbr, double addr, double iseq, double ratio)
{
    double best_score = HUGE_VAL;
    size_t best_i = 0;

    const context &ctx = contexts[bbr.context_index];
    const bbdef &bbd = bbdefs[ctx.bbdef_index];
    for (size_t i = 0; i < bbr.n_addrs; i++)
        if (bbr.addrs[i])
        {
            double addr_score = (bbr.addrs[i] - addr) * ratio;
            uint64_t cur_iseq = bbr.iseq_start + bbd.accesses[i].iseq;
            double score = hypot(addr_score, cur_iseq - iseq);
            if (score < best_score)
            {
                best_score = score;
                best_i = i;
            }
        }
    return make_pair(best_score, best_i);
}

mem_access dg_view_nearest_access(double addr, double iseq, double ratio)
{
    /* Start at the right instruction and search outwards until we can bound
     * the search.
     */
    vector<bbrun>::const_iterator back, forw, best = bbruns.end();
    size_t best_i = 0;
    double best_score = HUGE_VAL;

    forw = lower_bound(bbruns.begin(), bbruns.end(), (uint64_t) iseq, compare_bbrun_iseq());
    back = forw;
    best = forw;
    while (forw != bbruns.end() || back != bbruns.begin())
    {
        if (forw != bbruns.end())
        {
            if (forw->iseq_start > iseq + best_score)
                forw = bbruns.end();
            else
            {
                pair<double, size_t> sub = nearest_access_bbrun(*forw, addr, iseq, ratio);
                if (sub.first < best_score)
                {
                    best_score = sub.first;
                    best_i = sub.second;
                    best = forw;
                }
                forw++;
            }
        }
        if (back != bbruns.begin())
        {
            if (back->iseq_start <= iseq - best_score)
                back = bbruns.begin();
            else
            {
                --back;
                pair<double, size_t> sub = nearest_access_bbrun(*back, addr, iseq, ratio);
                if (sub.first < best_score)
                {
                    best_score = sub.first;
                    best_i = sub.second;
                    best = back;
                }
            }
        }
    }

    mem_access ans;

    if (best != bbruns.end())
    {
        const context &ctx = contexts[best->context_index];
        const bbdef &bbd = bbdefs[ctx.bbdef_index];
        assert(best_i < bbd.accesses.size());
        const bbdef_access &bbda = bbd.accesses[best_i];
        ans.addr = best->addrs[best_i];
        ans.dir = bbda.dir;
        ans.size = bbda.size;
        ans.block = best->blocks[best_i];

        assert(bbda.iseq < bbd.instr_addrs.size());
        ans.iseq = best->iseq_start + bbda.iseq;
        ans.stack = ctx.stack;
        if (ans.stack.empty())
            ans.stack.resize(1);
        ans.stack[0] = bbd.instr_addrs[bbda.iseq];
    }
    return ans;
}

const bbrun_list &dg_view_bbruns()
{
    return bbruns;
}

const bbdef &dg_view_bbrun_get_bbdef(const bbrun &bbr)
{
    assert(bbr.context_index < contexts.size());
    const context &ctx = contexts[bbr.context_index];
    assert(ctx.bbdef_index < bbdefs.size());
    return bbdefs[ctx.bbdef_index];
}

static mem_block *find_block(HWord addr)
{
    mem_block *block = NULL;
    rangemap<HWord, mem_block *>::iterator block_it = block_map.find(addr);
    if (block_it != block_map.end())
        block = block_it->second;
    return block;
}

static bool keep_access(HWord addr, uint8_t size, mem_block *block)
{
    bool matched;
    if (!chosen_events.empty() && active_events.empty())
        matched = false;
    else if (!chosen_ranges.empty())
    {
        matched = false;
        for (multiset<pair<HWord, HWord> >::const_iterator i = active_ranges.begin(); i != active_ranges.end(); ++i)
        {
            if (addr + size > i->first && addr < i->first + i->second)
            {
                matched = true;
                break;
            }
        }
        if (!matched && block != NULL)
        {
            /* Check for fn:, file: and dso: labels */
            for (size_t i = 0; i < block->stack.size(); i++)
            {
                string function, file, dso;
                int line;
                dg_view_addr2info(block->stack[i], function, file, line, dso);
                if (!function.empty())
                {
                    if (chosen_ranges.count("fn:" + function))
                    {
                        matched = true;
                        break;
                    }
                }
                if (!file.empty())
                {
                    file = dg_view_abbrev_file(file);
                    if (chosen_ranges.count("file:" + file))
                    {
                        matched = true;
                        break;
                    }
                }
                if (!dso.empty())
                {
                    dso = dg_view_abbrev_dso(dso);
                    if (chosen_ranges.count("dso:" + dso))
                    {
                        matched = true;
                        break;
                    }
                }
            }
        }
    }
    else
        matched = true;

    if (matched && malloc_only && block == NULL)
        matched = false;

    return matched;
}

bool dg_view_load(const char *filename)
{
    bool first = true;
    uint64_t iseq = 0;
    uint64_t dseq = 0;
    record_parser *rp_ptr;

    FILE *f = fopen(filename, "r");
    if (!f)
    {
        fprintf(stderr, "Could not open `%s'.\n", filename);
        return false;
    }
    while (NULL != (rp_ptr = record_parser::create(f)))
    {
        auto_ptr<record_parser> rp(rp_ptr);
        uint8_t type = rp->get_type();

        try
        {
            if (first)
            {
                uint8_t version, endian, wordsize;

                if (type != DG_R_HEADER)
                    throw record_parser_error("Error: did not find header");
                if (rp->extract_string() != "DATAGRIND1")
                    throw record_parser_error("Error: did not find signature");
                version = rp->extract_byte();
                endian = rp->extract_byte();
                wordsize = rp->extract_byte();
                int expected_version = 1;
                if (version != expected_version)
                {
                    fprintf(stderr, "Warning: version mismatch (expected %d, got %u).\n",
                            expected_version, version);
                }
                /* TODO: do something useful with endianness */
                if (wordsize != sizeof(HWord))
                {
                    ostringstream msg;
                    msg << "Error: pointer size mismatch (expected " << sizeof(HWord) << ", got " << wordsize << ")";
                    throw record_parser_content_error(msg.str());
                }

                first = false;
            }
            else
            {
                switch (type)
                {
                case DG_R_HEADER:
                    throw record_parser_content_error("Error: found header after first record.\n");

                case DG_R_BBDEF:
                    {
                        bbdef bbd;
                        uint8_t n_instrs = rp->extract_byte();
                        HWord n_accesses = rp->extract_word();

                        if (n_instrs == 0)
                        {
                            throw record_parser_content_error("Error: empty BB");
                        }
                        bbd.instr_addrs.resize(n_instrs);
                        bbd.accesses.resize(n_accesses);

                        for (HWord i = 0; i < n_instrs; i++)
                        {
                            bbd.instr_addrs[i] = rp->extract_word();
                            // discard size
                            (void) rp->extract_byte();
                        }
                        for (HWord i = 0; i < n_accesses; i++)
                        {
                            bbd.accesses[i].dir = rp->extract_byte();
                            bbd.accesses[i].size = rp->extract_byte();
                            bbd.accesses[i].iseq = rp->extract_byte();
                            if (bbd.accesses[i].iseq >= n_instrs)
                            {
                                throw record_parser_content_error("iseq is greater than instruction count");
                            }
                        }
                        bbdefs.push_back(bbd);
                    }
                    break;
                case DG_R_CONTEXT:
                    {
                        context ctx;
                        ctx.bbdef_index = rp->extract_word();

                        uint8_t n_stack = rp->extract_byte();
                        if (n_stack == 0)
                            throw record_parser_content_error("Error: empty call stack");
                        ctx.stack.resize(n_stack);
                        for (uint8_t i = 0; i < n_stack; i++)
                            ctx.stack[i] = rp->extract_word();

                        if (ctx.bbdef_index >= bbdefs.size())
                        {
                            ostringstream msg;
                            msg << "Error: bbdef index " << ctx.bbdef_index << " is out of range";
                            throw record_parser_content_error(msg.str());
                        }
                        contexts.push_back(ctx);
                    }
                    break;
                case DG_R_BBRUN:
                    {
                        bbrun bbr;
                        bool keep_any = false;

                        bbr.iseq_start = iseq;
                        bbr.dseq_start = dseq;
                        bbr.context_index = rp->extract_word();
                        if (bbr.context_index >= contexts.size())
                        {
                            ostringstream msg;
                            msg << "Error: context index " << bbr.context_index << " is out of range";
                            throw record_parser_content_error(msg.str());
                        }

                        const context &ctx = contexts[bbr.context_index];
                        const bbdef &bbd = bbdefs[ctx.bbdef_index];
                        uint8_t n_instrs = rp->extract_byte();
                        uint64_t n_addrs = rp->remain() / sizeof(HWord);
                        if (n_addrs > bbd.accesses.size())
                            throw record_parser_content_error("Error: too many access addresses");

                        bbr.n_addrs = n_addrs;
                        bbr.addrs = hword_pool.alloc(n_addrs);
                        bbr.blocks = mem_block_ptr_pool.alloc(n_addrs);
                        for (HWord i = 0; i < n_addrs; i++)
                        {
                            HWord addr = rp->extract_word();
                            const bbdef_access &access = bbd.accesses[i];

                            mem_block *block = find_block(addr);
                            bool keep = keep_access(addr, access.size, block);
                            if (keep)
                            {
                                keep_any = true;
                                fwd_page_map[page_round_down(addr)] = 0;
                                bbr.addrs[i] = addr;
                                bbr.blocks[i] = block;
                            }
                            else
                            {
                                bbr.addrs[i] = 0;
                                bbr.blocks[i] = NULL;
                            }
                        }

                        if (keep_any)
                            bbruns.push_back(bbr);
                        iseq += n_instrs;
                        dseq += n_addrs;
                    }
                    break;
                case DG_R_TRACK_RANGE:
                    {
                        HWord addr = rp->extract_word();
                        HWord size = rp->extract_word();

                        string var_type = rp->extract_string();
                        string label = rp->extract_string();

                        if (chosen_ranges.count(label))
                            active_ranges.insert(make_pair(addr, size));
                    }
                    break;
                case DG_R_UNTRACK_RANGE:
                    {
                        HWord addr = rp->extract_word();
                        HWord size = rp->extract_word();

                        pair<HWord, HWord> key(addr, size);
                        multiset<pair<HWord, HWord> >::iterator it = active_ranges.find(key);
                        if (it != active_ranges.end())
                            active_ranges.erase(it);
                    }
                    break;
                case DG_R_MALLOC_BLOCK:
                    {
                        HWord addr = rp->extract_word();
                        HWord size = rp->extract_word();
                        HWord n_ips = rp->extract_word();
                        vector<HWord> ips;

                        mem_block *block = new mem_block;
                        block->addr = addr;
                        block->size = size;
                        block->stack.reserve(n_ips);
                        for (HWord i = 0; i < n_ips; i++)
                        {
                            HWord stack_addr = rp->extract_word();
                            block->stack.push_back(stack_addr);
                        }
                        block_storage.push_back(block);
                        block_map.insert(addr, addr + size, block);
                    }
                    break;
                case DG_R_FREE_BLOCK:
                    {
                        HWord addr = rp->extract_word();
                        block_map.erase(addr);
                    }
                    break;
                case DG_R_START_EVENT:
                case DG_R_END_EVENT:
                    {
                        string label = rp->extract_string();
                        if (chosen_events.count(label))
                        {
                            if (type == DG_R_START_EVENT)
                                active_events.insert(label);
                            else
                            {
                                multiset<string>::iterator it = active_events.find(label);
                                if (it != active_events.end())
                                    active_events.erase(it);
                            }
                        }
                    }
                    break;
                case DG_R_TEXT_AVMA:
                    {
                        HWord avma = rp->extract_word();
                        string filename = rp->extract_string();
                        dg_view_load_object_file(filename.c_str(), avma);
                    }
                    break;
                default:
                    {
                        ostringstream msg;
                        msg << showbase << hex;
                        msg << "Error: unknown record type " << (unsigned int) type;
                        throw record_parser_content_error(msg.str());
                    }
                }
            }
            rp->finish();
        }
        catch (record_parser_content_error &e)
        {
            fprintf(stderr, "%s\n", e.what());
            rp->discard();
        }
        catch (record_parser_error &e)
        {
            fprintf(stderr, "%s\n", e.what());
            return false;
        }
    }
    fclose(f);

    /* bbruns is easily the largest structure, and due to the way vectors
     * work, could be overcommitted. Shrink back to just fit. */
    vector<bbrun> tmp(bbruns.begin(), bbruns.end());
    bbruns.swap(tmp);

    size_t remapped_base = 0;
    for (map<HWord, size_t>::iterator i = fwd_page_map.begin(); i != fwd_page_map.end(); i++)
    {
        i->second = remapped_base;
        remapped_base += DG_VIEW_PAGE_SIZE;
        rev_page_map[i->second] = i->first;
    }

    if (bbruns.empty())
    {
        fprintf(stderr, "No accesses match the criteria.\n");
        return false;
    }

#if 1
    printf("  %zu bbdefs\n"
           "  %zu bbruns\n"
           "  %zu contexts\n"
           "  %zu instrs (approx)\n"
           "  %zu accesses\n",
           bbdefs.size(),
           bbruns.size(),
           contexts.size(),
           bbruns.back().iseq_start,
           bbruns.back().dseq_start + bbruns.back().n_addrs);
#endif
    return true;
}

size_t dg_view_remap_address(HWord a)
{
    HWord base = page_round_down(a);
    map<HWord, size_t>::const_iterator it = fwd_page_map.find(base);
    assert(it != fwd_page_map.end());
    return (a - base) + it->second;
}

HWord dg_view_revmap_addr(size_t addr)
{
    HWord remapped_page = page_round_down(addr);
    if (!rev_page_map.count(remapped_page))
        return 0;
    HWord page = rev_page_map[remapped_page];
    HWord addr2 = (addr - remapped_page) + page;
    return addr2;
}

const page_map &dg_view_page_map()
{
    return fwd_page_map;
}

void dg_view_usage(const char *prog, int code)
{
    fprintf(stderr, "Usage: %s [-m] [--ranges=r1,...] [--events=e1,...] <file>\n", prog);
    exit(code);
}

/* Splits a string to pieces on commas. Empty parts are preserved, but if
 * s is empty then no strings are returned.
 */
static vector<string> split_comma(const string &s)
{
    vector<string> out;
    string::size_type pos = 0;

    if (s.empty()) return out;
    while (true)
    {
        string::size_type next = s.find(',', pos);
        if (next == string::npos)
        {
            /* End of string - capture the last element */
            out.push_back(s.substr(pos));
            return out;
        }
        out.push_back(s.substr(pos, next - pos));
        pos = next + 1;
    }
}

void dg_view_parse_opts(int *argc, char **argv)
{
    static const struct option longopts[] =
    {
        { "ranges", 1, NULL, 'r' },
        { "events", 1, NULL, 'e' },
        { "malloc-only", 0, NULL, 'm' },
        { NULL, 0, NULL, 0 }
    };
    int opt;

    while ((opt = getopt_long(*argc, argv, "r:e:m", longopts, NULL)) != -1)
    {
        switch (opt)
        {
        case 'r':
            {
                vector<string> ranges = split_comma(optarg);
                chosen_ranges = set<string>(ranges.begin(), ranges.end());
            }
            break;
        case 'e':
            {
                vector<string> events = split_comma(optarg);
                chosen_events = set<string>(events.begin(), events.end());
            }
            break;
        case 'm':
            malloc_only = true;
            break;
        case '?':
        case ':':
            exit(2);
        default:
            assert(0);
        }
    }

    /* Remove options from argv */
    for (int i = optind; i < *argc; i++)
        argv[i - optind + 1] = argv[i];
    *argc -= optind - 1;
}
