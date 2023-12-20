#include <iostream>
#include <string>
#include <vector>

struct number {
    int linenum;
    int start;
    int end;
    int value;
};

struct gear {
    int linenum;
    int pos;
};

struct parsed {
    std::vector<number> numbers;
    std::vector<gear> gears;
};

bool range_adjacent(int first_pos, int second_pos, int cmp)
{
    for (int i = first_pos - 1; i <= second_pos + 1; ++i) {
        if (i == cmp)
            return true;
    }
    return false;
}

bool adjacent(int first, int second)
{
    return first == second - 1 || first == second || first == second + 1;
}

int solve(const parsed& parsed)
{
    int sum = 0;

    for (auto& gear : parsed.gears) {
        std::vector<int> adjacent_numbers;

        for (auto& number : parsed.numbers)
            if (adjacent(gear.linenum, number.linenum) &&
                range_adjacent(number.start, number.end, gear.pos))
                adjacent_numbers.push_back(number.value);
        if (adjacent_numbers.size() == 2)
            sum += adjacent_numbers[0] * adjacent_numbers[1];
    }

    return sum;
}

parsed parse(const std::vector<std::string>& content)
{
    parsed parsed;

    for (auto line = content.begin(); line != content.end(); ++line) {
        std::string number_str;
        int nstart;
        bool in_number = false;

        for (auto ch = line->begin(); ch != line->end(); ++ch) {
            if (isdigit(*ch)) {
                number_str += *ch;
                if (!in_number)
                    nstart = ch - line->begin();
                in_number = true;
            } else {
                if (in_number) {
                    int linenum = line - content.begin();
                    int nend = ch - line->begin() - 1;
                    parsed.numbers.push_back(
                        {linenum, nstart, nend, stoi(number_str)});
                }
                number_str = "";
                in_number = false;

                if (*ch == '*') {
                    int linenum = line - content.begin();
                    int pos = ch - line->begin();
                    parsed.gears.push_back({linenum, pos});
                }
            }
        }

        // check if we were in a number, to avoid a bug where we don't
        // consider numbers at the end of lines
        if (in_number) {
            int linenum = line - content.begin();
            int nend = line->size() - 1;
            parsed.numbers.push_back({linenum, nstart, nend, stoi(number_str)});
        }
    }

    return parsed;
}

int main()
{
    std::vector<std::string> content;

    std::string line;
    while (std::getline(std::cin, line))
        content.push_back(line);

    std::cout << solve(parse(content)) << '\n';
}
