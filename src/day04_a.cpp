#include "aoc.h"
#include <algorithm>
#include <cmath>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

int solve(
    const std::vector<std::pair<std::vector<int>, std::vector<int>>>& parsed)
{
    int points{0};

    for (auto& game : parsed) {
        std::vector<int> intersections;

        std::set_intersection(game.first.begin(), game.first.end(),
                              game.second.begin(), game.second.end(),
                              std::back_inserter(intersections));

        if (intersections.size() == 1)
            points++;
        else if (intersections.size() > 1)
            points += std::pow(2, intersections.size() - 1);
    }

    return points;
}

std::pair<std::vector<int>, std::vector<int>>
parse_one(const std::string& input)
{
    std::vector<std::string> spl{split(input, " | ")};
    std::vector<int> winning{extract_integers(spl[0])};
    std::vector<int> have{extract_integers(spl[1])};

    std::sort(winning.begin(), winning.end());
    std::sort(have.begin(), have.end());

    return std::make_pair(winning, have);
}

std::vector<std::pair<std::vector<int>, std::vector<int>>>
parse_many(const std::vector<std::string>& content)
{
    std::vector<std::pair<std::vector<int>, std::vector<int>>> ret;
    for (auto& line : content)
        ret.push_back(parse_one(split(line, ": ")[1]));
    return ret;
}

int main()
{
    std::vector<std::string> content;

    std::string line;
    while (std::getline(std::cin, line))
        content.push_back(line);

    std::cout << solve(parse_many(content)) << '\n';
}
