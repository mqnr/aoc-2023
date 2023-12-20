#include "aoc.h"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <utility>
#include <vector>

using std::begin;
using std::end;

struct range {
    std::pair<long long, long long> source;
    std::pair<long long, long long> destination;
};

using mapping = std::vector<range>;

struct parsed {
    std::vector<long long> initial_seeds;
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

long long solve(parsed& parsed)
{
    std::vector<long long> locations;
    for (long long seed : parsed.initial_seeds)
        locations.push_back(location_for_seed(parsed.maps, seed));

    return *std::min_element(begin(locations), end(locations));
}

range make_range(const std::vector<long long>& trio)
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
    parsed_result.initial_seeds = extract_lls(split(spl[0], ": ")[1]);

    spl.erase(begin(spl));

    for (auto it = begin(spl); it != end(spl); ++it) {
        std::vector<std::string> lines{split_newline(*it)};
        lines.erase(begin(lines));

        mapping map;
        for (auto const& l : lines)
            map.push_back(make_range(extract_lls(l)));

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
    std::cout << solve(parsed_result) << '\n';
}
