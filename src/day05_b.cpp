#include "aoc.h"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <limits>
#include <thread>
#include <utility>
#include <vector>

struct range {
    std::pair<long long, long long> source;
    std::pair<long long, long long> destination;
};

using mapping = std::vector<range>;

struct parsed {
    std::vector<std::pair<long long, long long>> initial_seeds;
    std::vector<mapping> maps;
};

bool in_range(long long n, std::pair<long long, long long> range)
{
    return (n >= range.first) && (n <= range.second);
}

long long location_for_seed(const std::vector<mapping>& mappings,
                            long long seed)
{
    long long val{seed};

    for (auto const& map : mappings)
        for (auto const& range : map)
            if (in_range(val, range.source)) {
                long long diff{range.source.second - val};
                val = range.destination.second - diff;
                break;
            }

    return val;
}

long long parallel_solve(const parsed& parsed_data, int num_threads)
{
    std::vector<std::thread> threads;
    std::vector<long long> local_minima(num_threads,
                                        std::numeric_limits<long long>::max());

    auto threadFunc = [&](int thread_id) {
        for (size_t i = thread_id; i < parsed_data.initial_seeds.size();
             i += num_threads)
            for (long long seed = parsed_data.initial_seeds[i].first;
                 seed <= parsed_data.initial_seeds[i].second; ++seed) {
                long long location = location_for_seed(parsed_data.maps, seed);
                if (location < local_minima[thread_id])
                    local_minima[thread_id] = location;
            }
    };

    for (int i = 0; i < num_threads; ++i)
        threads.emplace_back(threadFunc, i);

    for (auto& t : threads)
        t.join();

    long long global_min = std::numeric_limits<long long>::max();
    for (long long min : local_minima)
        if (min < global_min)
            global_min = min;

    return global_min;
}

std::pair<long long, long long> make_range2(long long first, long long second)
{
    std::pair<long long, long long> pair{
        std::make_pair(first, first + (second - 1))};
    return pair;
}

range make_range3(const std::vector<long long>& trio)
{
    std::pair<long long, long long> source{
        std::make_pair(trio[1], trio[1] + (trio[2] - 1))};
    std::pair<long long, long long> destination{
        std::make_pair(trio[0], trio[0] + (trio[2] - 1))};

    return range{source, destination};
}

parsed parse(const std::string& content)
{
    parsed parsed_result;

    std::vector<std::string> spl = split(remove_suffix(content, "\n"), "\n\n");

    std::vector<long long> raw_seed_pairs = extract_lls(split(spl[0], ": ")[1]);
    std::vector<std::pair<long long, long long>> seed_pairs;
    for (auto it = std::begin(raw_seed_pairs); it != std::end(raw_seed_pairs);
         ++it)
        if ((it - std::begin(raw_seed_pairs)) % 2 == 0)
            seed_pairs.push_back(make_range2(*it, *std::next(it)));

    parsed_result.initial_seeds = seed_pairs;

    spl.erase(std::begin(spl));

    for (auto it = std::begin(spl); it != std::end(spl); ++it) {
        std::vector<std::string> lines{split_newline(*it)};
        lines.erase(std::begin(lines));

        mapping map;
        for (auto const& l : lines)
            map.push_back(make_range3(extract_lls(l)));

        parsed_result.maps.push_back(map);
    }

    return parsed_result;
}

int main()
{
    std::string content;

    std::string line;
    while (std::getline(std::cin, line))
        content += line + '\n';

    parsed parsed_result{parse(content)};
    std::cout << parallel_solve(parsed_result, 6) << '\n';
}
