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

#define __STDC_FORMAT_MACROS 1
#include <inttypes.h>
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

#define CONDITION_SET_FLAG_ANY           1U
#define CONDITION_SET_FLAG_MALLOC        2U /* Any malloc-like function */
#define CONDITION_SET_FLAG_RANGE         4U /* Any TRACK_RANGE request */

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
template<typename WordType>
pair<double, size_t> dg_view<WordType>::nearest_access_bbrun(const bbrun &bbr, double addr, double iseq, double ratio) const
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

template<typename WordType>
mem_access dg_view<WordType>::dg_view_nearest_access(double addr, double iseq, double ratio) const
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

template<typename WordType>
const dg_view<WordType>::bbrun_list & dg_view<WordType>::get_bbruns() const
{
    return bbruns;
}

template<typename WordType>
const dg_view<WordType>::bbdef &dg_view<WordType>::bbrun_get_bbdef(const bbrun &bbr) const
{
    assert(bbr.context_index < contexts.size());
    const context &ctx = contexts[bbr.context_index];
    assert(ctx.bbdef_index < bbdefs.size());
    return bbdefs[ctx.bbdef_index];
}

template<typename WordType>
dg_view<WordType>::mem_block *dg_view<WordType>::find_block(word_type addr) const
{
    mem_block *block = NULL;
    rangemap<HWord, mem_block *>::const_iterator block_it = block_map.find(addr);
    if (block_it != block_map.end())
        block = block_it->second;
    return block;
}

/* Checks whether a condition specified by a condition_set is met. It does not
 * include user events or the flags.
 */
template<typename WordType>
bool dg_view<WordType>::condition_set_match(const condition_set &cs, const vector<word_type> &stack) const
{
    if (cs.functions.empty() && cs.files.empty() && cs.dsos.empty())
        return false;
    for (size_t i = 0; i < stack.size(); i++)
    {
        map<word_type, bool>::iterator pos = cs.cache.lower_bound(stack[i]);
        if (pos != cs.cache.end() && pos->first == stack[i])
        {
            if (pos->second)
                return true;
            else
                continue;
        }

        bool match = false;
        string function, file, dso;
        int line;
        dg_view_addr2info(stack[i], function, file, line, dso);
        if ((!function.empty() && cs.functions.count(function))
            || (!file.empty() && cs.files.count(file))
            || (!dso.empty() && cs.dsos.count(dso)))
        {
            match = true;
        }
        cs.cache.insert(pos, make_pair(stack[i], match));
        if (match)
            return true;
    }

    return false;
}

/* Whether a memory access matches the range conditions */
template<typename WordType>
bool dg_view<WordType>::keep_access_block(word_type addr, uint8_t size, mem_block *block) const
{
    if (block_conditions.flags & CONDITION_SET_FLAG_ANY)
        return true;
    if (block != NULL && block->matched)
        return true;

    for (multiset<pair<word_type, word_type> >::const_iterator i = active_ranges.begin(); i != active_ranges.end(); ++i)
        if (addr + size > i->first && addr < i->first + i->second)
            return true;

    return false;
}

/* Whether a memory access matches the event conditions */
template<typename WordType>
bool dg_view<WordType>::keep_access_event(const vector<word_type> &stack) const
{
    if (event_conditions.flags & CONDITION_SET_FLAG_ANY)
        return true;
    if (!active_events.empty())
        return true;

    return condition_set_match(event_conditions, stack);
}

template<typename WordType>
bool dg_view<WordType>::keep_access(word_type addr, uint8_t size, const vector<word_type> &stack, mem_block *block) const
{
    return keep_access_block(addr, size, block) && keep_access_event(stack);
}

template<typename WordType>
void dg_view<WordType>::get_ranges(address_type &addr_min, address_type &addr_max, iseq_type &iseq_min, iseq_type &iseq_max) const
{
    addr_min = fwd_page_map.begin()->second;
    addr_max = (--fwd_page_map.end())->second + DG_VIEW_PAGE_SIZE;
    iseq_min = bbruns.begin()->iseq_start;

    bbrun_list::const_iterator last_bbrun = --bbruns.end();
    const bbdef &last_bbdef = bbrun_get_bbdef(*last_bbrun);
    iseq_max = last_bbrun->iseq_start + last_bbdef.accesses.back().iseq + 1;
}

template<typename WordType>
dg_view_base *dg_view<WordType>::load_internal(FILE *f, const char *filename, int version, int endian)
{
    uint64_t iseq = 0;
    uint64_t dseq = 0;
    record_parser *rp_ptr;

    while (NULL != (rp_ptr = record_parser::create(f)))
    {
        auto_ptr<record_parser> rp(rp_ptr);
        uint8_t type = rp->get_type();

        try
        {
            switch (type)
            {
            case DG_R_HEADER:
                throw record_parser_content_error("Error: found header after first record.\n");

            case DG_R_BBDEF:
                {
                    bbdef bbd;
                    uint8_t n_instrs = rp->extract_byte();
                    HWord n_accesses = rp->extract_word<word_type>();

                    if (n_instrs == 0)
                    {
                        throw record_parser_content_error("Error: empty BB");
                    }
                    bbd.instr_addrs.resize(n_instrs);
                    bbd.accesses.resize(n_accesses);

                    for (word_type i = 0; i < n_instrs; i++)
                    {
                        bbd.instr_addrs[i] = rp->extract_word<word_type>();
                        // discard size
                        (void) rp->extract_byte();
                    }
                    for (word_type i = 0; i < n_accesses; i++)
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
                    ctx.bbdef_index = rp->extract_word<word_type>();

                    uint8_t n_stack = rp->extract_byte();
                    if (n_stack == 0)
                        throw record_parser_content_error("Error: empty call stack");
                    ctx.stack.resize(n_stack);
                    for (uint8_t i = 0; i < n_stack; i++)
                        ctx.stack[i] = rp->extract_word<word_type>();

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
                    bbr.context_index = rp->extract_word<word_type>();
                    if (bbr.context_index >= contexts.size())
                    {
                        ostringstream msg;
                        msg << "Error: context index " << bbr.context_index << " is out of range";
                        throw record_parser_content_error(msg.str());
                    }

                    const context &ctx = contexts[bbr.context_index];
                    const bbdef &bbd = bbdefs[ctx.bbdef_index];
                    uint8_t n_instrs = rp->extract_byte();
                    uint64_t n_addrs = rp->remain() / sizeof(word_type);
                    if (n_addrs > bbd.accesses.size())
                        throw record_parser_content_error("Error: too many access addresses");

                    bbr.n_addrs = n_addrs;
                    bbr.addrs = hword_pool.alloc(n_addrs);
                    bbr.blocks = mem_block_ptr_pool.alloc(n_addrs);
                    vector<word_type> stack = ctx.stack;
                    for (word_type i = 0; i < n_addrs; i++)
                    {
                        word_type addr = rp->extract_word<word_type>();
                        const bbdef_access &access = bbd.accesses[i];

                        mem_block *block = find_block(addr);
                        stack[0] = bbd.instr_addrs[access.iseq];
                        bool keep = keep_access(addr, access.size, stack, block);
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
                    word_type addr = rp->extract_word<word_type>();
                    word_type size = rp->extract_word<word_type>();

                    string var_type = rp->extract_string();
                    string label = rp->extract_string();

                    if (block_conditions.user.count(label)
                        || (block_conditions.flags & CONDITION_SET_FLAG_RANGE))
                        active_ranges.insert(make_pair(addr, size));
                }
                break;
            case DG_R_UNTRACK_RANGE:
                {
                    word_type addr = rp->extract_word<word_type>();
                    word_type size = rp->extract_word<word_type>();

                    pair<word_type, word_type> key(addr, size);
                    multiset<pair<word_type, word_type> >::iterator it = active_ranges.find(key);
                    if (it != active_ranges.end())
                        active_ranges.erase(it);
                }
                break;
            case DG_R_MALLOC_BLOCK:
                {
                    word_type addr = rp->extract_word<word_type>();
                    word_type size = rp->extract_word<word_type>();
                    word_type n_ips = rp->extract_word<word_type>();
                    vector<word_type> ips;

                    mem_block *block = new mem_block;
                    block->addr = addr;
                    block->size = size;
                    block->stack.reserve(n_ips);
                    for (word_type i = 0; i < n_ips; i++)
                    {
                        word_type stack_addr = rp->extract_word<word_type>();
                        block->stack.push_back(stack_addr);
                    }
                    block->matched = false;
                    if (block_conditions.flags & (CONDITION_SET_FLAG_ANY | CONDITION_SET_FLAG_MALLOC)
                        || condition_set_match(block_conditions, block->stack))
                        block->matched = true;
                    block_storage.push_back(block);
                    block_map.insert(addr, addr + size, block);
                }
                break;
            case DG_R_FREE_BLOCK:
                {
                    word_type addr = rp->extract_word<word_type>();
                    block_map.erase(addr);
                }
                break;
            case DG_R_START_EVENT:
            case DG_R_END_EVENT:
                {
                    string label = rp->extract_string();
                    if (event_conditions.user.count(label))
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
                    word_type avma = rp->extract_word<word_type>();
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
            fprintf(stderr, "%s: %s\n", filename, e.what());
            rp->discard();
        }
    }

    /* bbruns is easily the largest structure, and due to the way vectors
     * work, could be overcommitted. Shrink back to just fit. */
    vector<bbrun> tmp(bbruns.begin(), bbruns.end());
    bbruns.swap(tmp);

    size_t remapped_base = 0;
    for (map<word_type, size_t>::iterator i = fwd_page_map.begin(); i != fwd_page_map.end(); i++)
    {
        i->second = remapped_base;
        remapped_base += DG_VIEW_PAGE_SIZE;
        rev_page_map[i->second] = i->first;
    }

    if (bbruns.empty())
    {
        throw record_parser_error("No accesses match the criteria.");
    }

#if 1
    printf("  %zu bbdefs\n"
           "  %zu bbruns\n"
           "  %zu contexts\n"
           "  %" PRIu64 " instrs (approx)\n"
           "  %" PRIu64 " accesses\n",
           bbdefs.size(),
           bbruns.size(),
           contexts.size(),
           bbruns.back().iseq_start,
           bbruns.back().dseq_start + bbruns.back().n_addrs);
#endif
}

dg_view_base *dg_view_load(const char *filename)
{
    record_parser *rp_ptr;
    auto_ptr<dg_view_base> accesses;
    FILE *f = fopen(filename, "r");
    if (!f)
    {
        fprintf(stderr, "Could not open `%s'.\n", filename);
        return NULL;
    }

    try
    {
        uint8_t version, endian;
        try
        {
            if (NULL != (rp_ptr = record_parser::create(f)))
            {
                auto_ptr<record_parser> rp(rp_ptr);
                uint8_t type = rp->get_type();
                uint8_t wordsize;
                uint8_t expected_version = 1;

                if (type != DG_R_HEADER)
                    throw record_parser_error("Error: did not find header");
                if (rp->extract_string() != "DATAGRIND1")
                    throw record_parser_error("Error: did not find signature");
                version = rp->extract_byte();
                endian = rp->extract_byte();
                wordsize = rp->extract_byte();
                if (version != expected_version)
                {
                    fprintf(stderr, "Warning: version mismatch (expected %d, got %u).\n",
                            expected_version, version);
                }
                /* TODO: do something useful with endianness */

                switch (wordsize)
                {
                case 4:
                    accesses = new dg_view<uint32_t>;
                    break;
                case 8:
                    accesses = new dg_view<uint64_t>;
                    break;
                default:
                    {
                        ostringstream msg;
                        msg << "Error: unsupported word size (got " << wordsize << ", expected 4 or 8)";
                        throw record_parser_error(msg.str());
                    }
                }
            }
            else
            {
                throw record_parser_error("Error: empty or unreadable file");
            }
        }
        catch (record_parser_content_error &e)
        {
            fprintf(stderr, "%s: %s\n", filename, e.what());
            rp->discard();
        }

        accesses->load_internal(version, endian);
    }
    catch (record_parser_error &e)
    {
        fprintf(stderr, "%s: %s\n", filename, e.what());
        fclose(f);
        return NULL;
    }
    catch (...)
    {
        fclose(f);
        throw;
    }

    fclose(f);
    return accesses.release();
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

static void condition_set_parse(condition_set &cs, const vector<string> &tokens, uint32_t valid_flags)
{
    for (vector<string>::const_iterator i = tokens.begin(); i != tokens.end(); ++i)
    {
        const string &s = *i;
        if ((valid_flags & CONDITION_SET_FLAG_MALLOC) && s == "malloc")
            cs.flags |= CONDITION_SET_FLAG_MALLOC;
        else if ((valid_flags & CONDITION_SET_FLAG_RANGE) && s == "range")
            cs.flags |= CONDITION_SET_FLAG_RANGE;
        else if (s.substr(0, 3) == "fn:")
            cs.functions.insert(s.substr(3));
        else if (s.substr(0, 5) == "file:")
            cs.files.insert(s.substr(5));
        else if (s.substr(0, 4) == "dso:")
            cs.dsos.insert(s.substr(4));
        else if (s.substr(0, 5) == "user:")
            cs.user.insert(s.substr(5));
        else
        {
            fprintf(stderr, "Invalid condition `%s'\n", s.c_str());
            exit(2);
        }
    }
}

void dg_view_parse_opts(int *argc, char **argv)
{
    static const struct option longopts[] =
    {
        { "ranges", 1, NULL, 'r' },
        { "events", 1, NULL, 'e' },
        { NULL, 0, NULL, 0 }
    };
    int opt;

    event_conditions.flags = CONDITION_SET_FLAG_ANY;
    block_conditions.flags = CONDITION_SET_FLAG_ANY;
    while ((opt = getopt_long(*argc, argv, "r:e:", longopts, NULL)) != -1)
    {
        switch (opt)
        {
        case 'r':
            {
                vector<string> ranges = split_comma(optarg);
                condition_set_parse(block_conditions, ranges, CONDITION_SET_FLAG_MALLOC | CONDITION_SET_FLAG_RANGE);
                block_conditions.flags &= ~CONDITION_SET_FLAG_ANY;
            }
            break;
        case 'e':
            {
                vector<string> events = split_comma(optarg);
                condition_set_parse(event_conditions, events, 0);
                event_conditions.flags &= ~CONDITION_SET_FLAG_ANY;
            }
            break;
        case '?':
        case ':':
            exit(2);
        default:
            assert(0);
        }
    }

    event_conditions.cache.clear();
    block_conditions.cache.clear();

    /* Remove options from argv */
    for (int i = optind; i < *argc; i++)
        argv[i - optind + 1] = argv[i];
    *argc -= optind - 1;
}
