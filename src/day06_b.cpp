#include "aoc.h"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <limits>
#include <string>
#include <thread>
#include <utility>
#include <vector>

long travel_race(long race_duration, long ms)
{
    return ms * (race_duration - ms);
}

long ways_to_win(long race_duration, long record)
{
    long wins = 0;
    for (long i = 0; i <= race_duration; i++)
        if (travel_race(race_duration, i) > record)
            wins++;
    return wins;
}

long solve(const std::pair<long, long> race)
{
    return ways_to_win(race.first, race.second);
}

std::pair<long, long> parse(const std::vector<std::string>& content)
{
    std::vector<std::pair<long, long>> races;

    std::string line1 = after_or_same(content[0], ": ");
    std::string line2 = after_or_same(content[1], ": ");

    std::vector<std::string> time_split = split_whitespace(line1);
    std::vector<std::string> distance_split = split_whitespace(line2);

    std::string time;
    std::string distance;
    for (const auto& s : time_split)
        time += s;
    for (const auto& s : distance_split)
        distance += s;

    return std::make_pair(std::stol(time), std::stol(distance));
}

int main()
{
    std::vector<std::string> content;

    std::string line;
    while (std::getline(std::cin, line))
        content.push_back(line);

    std::cout << solve(parse(content)) << '\n';
}
