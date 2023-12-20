#include <cctype>
#include <iostream>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

int main()
{
    string line;
    vector<pair<int, int>> symbols;
    vector<string> table;

    int linenum = 0;
    while (getline(cin, line)) {
        table.push_back("");
        int charnum = 0;
        for (char& c : line) {
            if (c != '.' && !isdigit(c))
                symbols.push_back(make_pair(linenum, charnum));
            table[table.size() - 1] += c;
            charnum++;
        }
        linenum++;
    }

    int sum = 0;
    for (size_t i = 0; i < table.size(); ++i) {
        line = table[i];
        bool in_number = false;
        string number;
        int nstart = -1;
        for (size_t j = 0; j < line.size(); ++j) {
            if (isdigit(line[j])) {
                if (in_number) {
                    number += line[j];
                } else {
                    number = line[j];
                    nstart = j;
                }
                in_number = true;
            }

            if (!isdigit(line[j]) || j == line.size() - 1) {
                if (!in_number) {
                    continue;
                }
                bool found = false;
                for (auto pair : symbols) {
                    if (found)
                        break;
                    if (static_cast<unsigned>(pair.first) == i - 1 ||
                        static_cast<unsigned>(pair.first) == i ||
                        static_cast<unsigned>(pair.first) == i + 1)
                        for (int k = nstart - 1; k <= static_cast<int>(j); ++k)
                            if (k == pair.second) {
                                sum += stoi(number);
                                found = true;
                                break;
                            }
                }
                nstart = -1;
                number = "";
                in_number = false;
            }
        }
    }

    cout << sum << '\n';

    return 0;
}
