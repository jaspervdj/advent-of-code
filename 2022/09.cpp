#include <fstream>
#include <iostream>
#include <unordered_set>
#include <vector>

struct pos {
    int x;
    int y;

    pos() {
        this->x = 0;
        this->y = 0;
    }

    inline void move(const char dir) noexcept {
        switch(dir) {
        case 'U':
            this->y--;
            break;
        case 'R':
            this->x++;
            break;
        case 'D':
            this->y++;
            break;
        case 'L':
            this->x--;
            break;
        }
    }

    inline void drag(pos const& head) noexcept {
        const int dx = head.x - this->x;
        const int dy = head.y - this->y;
        if (dx >= -1 && dx <= 1 && dy >= -1 && dy <= 1) {
            return;
        }
        if (dx != 0) {
            this->x += dx > 0 ? 1 : -1;
        }
        if (dy != 0) {
            this->y += dy > 0 ? 1 : -1;
        }
    }
};

struct pos_equal {
    bool operator()(pos const &l, pos const &r) const noexcept {
        return l.x == r.x && l.y == r.y;
    };
};

struct pos_hash {
    std::size_t operator()(pos const &pos) const noexcept {
        const std::size_t h1 = std::hash<int>{}(pos.x);
        const std::size_t h2 = std::hash<int>{}(pos.y);
        return h1 ^ (h2 << 1);
    }
};

struct rope {
    size_t n;
    std::vector<pos> knots;

    rope(size_t n) noexcept {
        this->n = n;
        for (size_t i = 0; i < n; i++) {
            this->knots.push_back(pos());
        }
    }

    inline void step(const char dir) noexcept {
        this->knots[0].move(dir);
        for (size_t i = 1; i < this->n; i++) {
            this->knots[i].drag(this->knots[i - 1]);
        }
    }

    inline pos &tail() noexcept {
        return this->knots[this->n - 1];
    }
};

std::vector<std::pair<char, int>> parse_commands(std::istream &input) {
    std::vector<std::pair<char, int>> commands;
    char command;
    int count;
    while (input >> command >> count) {
        commands.push_back(std::pair(command, count));
    }
    return commands;
}

int solve(std::vector<std::pair<char, int>> commands, size_t rope_size) {
    std::unordered_set<pos, pos_hash, pos_equal> visited;
    rope rope(rope_size);

    for (auto it = std::begin(commands); it != std::end(commands); ++it) {
        char command = it->first;
        int count = it->second;
        for (int i = 0; i < count; i++) {
            rope.step(command);
            visited.insert(rope.tail());
        }
    }

    return visited.size();
}

int main(int argc, char **argv) {
    std::vector<std::pair<char, int>> commands;
    bool run_part_1 = true;
    bool run_part_2 = true;

    if (argc == 1) {
        commands = parse_commands(std::cin);
    } else if (argc == 4) {
        std::ifstream input(argv[3]);
        commands = parse_commands(input);
        run_part_1 = std::string(argv[2]) == "1";
        run_part_2 = std::string(argv[2]) == "2";
    } else {
        std::cerr << "Usage: " << argv[0] << " [DAY] [PART] [INPUT]" << std::endl;
        return 1;
    }

    if (run_part_1) std::cout << solve(commands, 2) << std::endl;
    if (run_part_2) std::cout << solve(commands, 10) << std::endl;
    return 0;
}
