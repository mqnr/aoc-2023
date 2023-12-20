#include "aoc.h"
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

using namespace std;

int main()
{
    string line;
    int sum = 0;
    while (getline(cin, line)) {
        string gameline;
        int num;

        istringstream gs(line);
        getline(gs, gameline, ':');

        istringstream gss(gameline);
        gss >> gameline >> num;

        string showing;
        int max_red = 0;
        int max_green = 0;
        int max_blue = 0;
        while (getline(gs, showing, ';')) {
            vector<string> spl = split(showing, ",");

            for (auto& s : spl) {
                int amount;
                string color;

                s = remove_prefix(s, " ");
                istringstream shs(s);
                shs >> amount >> color;

                if (color == "red" && amount > max_red)
                    max_red = amount;
                if (color == "green" && amount > max_green)
                    max_green = amount;
                if (color == "blue" && amount > max_blue)
                    max_blue = amount;
            }
        }
        sum += max_red * max_green * max_blue;
    }

    cout << sum << '\n';

    return 0;
}
