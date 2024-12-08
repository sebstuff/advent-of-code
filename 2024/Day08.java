import java.io.*;
import java.util.*;

void main() throws Exception {
    var grid = loadGrid("day08.input");
    var antennaGroups = groupAntennas(grid);

    println("Total antinodes: " + totalAntinodes(antennaGroups, grid));
}

char[][] loadGrid(String filename) throws IOException {
    var grid = new ArrayList<char[]>();

    try (var in = new BufferedReader(new FileReader(filename))) {
        String line;

        while ((line = in.readLine()) != null) {
            grid.add(line.toCharArray());
        }
    }

    return grid.toArray(new char[grid.size()][]);
}

Map<Character, List<Position>> groupAntennas(char[][] grid) {
    var antennaGroups = new HashMap<Character, List<Position>>();

    for (var x = 0; x < grid.length; ++x) {
        for (var y = 0; y < grid.length; ++y) {
            var ch = grid[x][y];
            if (ch != '.') {
                var antennas = antennaGroups.get(ch);
                if (antennas == null) {
                    antennas = new ArrayList<>();
                    antennaGroups.put(ch, antennas);
                }
                antennas.add(new Position(x, y));
            }
        }
    }

    return antennaGroups;
}

int totalAntinodes(Map<Character, List<Position>> antennaGroups, char[][] grid) {
    var antinodes = new boolean[grid.length][grid[0].length];

    for (var antennas : antennaGroups.values()) {
        for (var i = 0; i < antennas.size(); ++i) {
            for (var j = 0; j < antennas.size(); ++j) {
                if (i == j) {
                    continue;
                }

                var a1 = antennas.get(i);
                var a2 = antennas.get(j);
                var x = a1.x + a1.x - a2.x;
                var y = a1.y + a1.y - a2.y;

                if (x > -1 && x < antinodes.length 
                    && y > -1 && y < antinodes[0].length) {
                    antinodes[x][y] = true;
                }
            }
        }
    }

    var totalAntinodes = 0;

    for (var x = 0; x < antinodes.length; ++x) {
        for (var y = 0; y < antinodes.length; ++y) {
            if (antinodes[x][y]) {
                ++totalAntinodes;
            }
        }
    }

    return totalAntinodes;
}

record Position(int x, int y) {}
