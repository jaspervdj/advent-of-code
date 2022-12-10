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

int main(int argc, char **argv) {
    if (argc != 4) {
        std::cerr << "Usage: " << argv[0] << " " << " [DAY] [PART] [INPUT]" << std::endl;
        return 1;
    }

    size_t rope_size = 2;
    if (std::string(argv[2]) == "2") {
        rope_size = 10;
    }

    std::unordered_set<pos, pos_hash, pos_equal> visited;
    rope rope(rope_size);

    std::ifstream infile(argv[3]);
    char command;
    int count;

    while (infile >> command >> count) {
        for (int i = 0; i < count; i++) {
            rope.step(command);
            visited.insert(rope.tail());
        }
    }

    std::cout << visited.size() << std::endl;
    return 0;
}
