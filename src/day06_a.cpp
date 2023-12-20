#include "aoc.h"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <limits>
#include <thread>
#include <utility>
#include <vector>

int travel_race(int race_duration, int ms) { return ms * (race_duration - ms); }

int ways_to_win(int race_duration, int record)
{
    int wins = 0;
    for (int i = 0; i <= race_duration; i++)
        if (travel_race(race_duration, i) > record)
            wins++;
    return wins;
}

int solve(const std::vector<std::pair<int, int>> races)
{
    int result = 0;

    for (auto const& race : races) {
        int ways = ways_to_win(race.first, race.second);
        if (result == 0)
            result = ways;
        else
            result *= ways;
    }

    return result;
}

std::vector<std::pair<int, int>> parse(const std::vector<std::string>& content)
{
    std::vector<std::pair<int, int>> races;

    std::string line1 = after_or_same(content[0], ": ");
    std::string line2 = after_or_same(content[1], ": ");
    std::vector<int> times = extract_integers(line1);
    std::vector<int> distances = extract_integers(line2);

    for (size_t i = 0; i < times.size(); i++)
        races.push_back(std::make_pair(times[i], distances[i]));

    return races;
}

int main()
{
    std::vector<std::string> content;

    std::string line;
    while (std::getline(std::cin, line))
        content.push_back(line);

    std::cout << solve(parse(content)) << '\n';
}
