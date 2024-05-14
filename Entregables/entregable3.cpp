#include <iostream>
#include <vector>
#include <cstdio>
#include <cstring>
using namespace std;

int acorn(vector<vector<int>> &positions, int t, int h, int f)
{
    vector<vector<int>> maximos(t, vector<int>(h + 1, 0));
    vector<int> max_per_height(h + 1, -1); // Initialize with negative infinity

    for (int altura = 1; altura <= h; altura++)
    {

        for (int i = 0; i < t; i++)
        {
            int max_altura = 0;

            if (altura - f >= 0)
            {
                max_altura = max(max_altura, max_per_height[altura - f]);
            }

            max_altura = max(maximos[i][altura - 1], max_altura);

            maximos[i][altura] = max(max_altura, positions[i][altura - 1]) + positions[i][altura];
            max_per_height[altura] = max(max_per_height[altura], maximos[i][altura]);
        }
    }

    return max_per_height[h];
}

int main()
{
    int casos;
    scanf("%d", &casos);

    while (casos--)
    {
        int t, h, f;
        scanf("%d %d %d", &t, &h, &f);

        vector<vector<int>> positions(t, vector<int>(h + 1, 0));

        for (int i = 0; i < t; ++i)
        {
            int amount;
            scanf("%d", &amount);

            for (int j = 0; j < amount; ++j)
            {
                int acorn;
                scanf("%d", &acorn);

                positions[i][acorn]++;
            }
        }

        int acorns = acorn(positions, t, h, f);
        printf("%d\n", acorns);
    }

    return 0;
}