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
        bool invalid = false;
        while (getline(gs, showing, ';')) {
            if (invalid)
                break;

            vector<string> spl = split(showing, ",");

            int red = 0;
            int green = 0;
            int blue = 0;
            for (auto& s : spl) {
                int amount;
                string color;

                s = remove_prefix(s, " ");
                istringstream shs(s);
                shs >> amount >> color;

                if (color == "red")
                    red += amount;
                if (color == "green")
                    green += amount;
                if (color == "blue")
                    blue += amount;
            }

            if (red > 12)
                invalid = true;
            if (green > 13)
                invalid = true;
            if (blue > 14)
                invalid = true;
        }
        if (!invalid)
            sum += num;
    }

    cout << sum << '\n';

    return 0;
}
