package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

type fs struct {
	size     int
	children map[string]*fs
}

func newFs() *fs {
	return &fs{
		size:     0,
		children: map[string]*fs{},
	}
}

func (root *fs) sizeUp() {
	root.bottomUp(func(fs *fs) {
		if len(fs.children) > 0 {
			fs.size = 0
			for _, child := range fs.children {
				fs.size += child.size
			}
		}
	})
}

func (fs *fs) bottomUp(f func(*fs)) {
	for _, child := range fs.children {
		child.bottomUp(f)
	}
	f(fs)
}

type parser struct {
	stack []*fs
}

func newParser(root *fs) *parser {
	return &parser{
		stack: []*fs{root},
	}
}

func (p *parser) command(cmd string) {
	tokens := strings.Split(cmd, " ")
	if len(tokens) == 3 && tokens[0] == "$" && tokens[1] == "cd" {
		if tokens[2] == "/" {
			p.stack = []*fs{p.stack[0]}
		} else if tokens[2] == ".." {
			p.stack = p.stack[:len(p.stack)-1]
		} else {
			child := p.stack[len(p.stack)-1].children[tokens[2]]
			p.stack = append(p.stack, child)
		}
		return
	} else if len(tokens) == 2 && tokens[0] == "$" && tokens[1] == "ls" {
		return
	} else if len(tokens) == 2 && tokens[0] == "dir" {
		dir := tokens[1]
		p.stack[len(p.stack)-1].children[dir] = newFs()
		return
	} else {
		if len(tokens) == 2 {
			if size, err := strconv.Atoi(tokens[0]); err == nil {
				file := newFs()
				file.size = size
				p.stack[len(p.stack)-1].children[tokens[1]] = file
				return
			}
		}
	}

	panic("unknown command: " + cmd)
}

func part1(root *fs) int {
	solution := 0
	root.bottomUp(func(fs *fs) {
		if fs.size <= 100000 && len(fs.children) > 0 {
			solution += fs.size
		}
	})
	return solution
}

func part2(root *fs) int {
	total := 70000000
	required := 30000000
	used := root.size
	unused := total - used
	needed := required - unused
	solution := total
	root.bottomUp(func(fs *fs) {
		if fs.size >= needed && fs.size < solution {
			solution = fs.size
		}
	})
	return solution
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	root := newFs()
	parser := newParser(root)
	for scanner.Scan() {
		parser.command(scanner.Text())
	}
	root.sizeUp()
	fmt.Fprintf(os.Stdout, "%d\n", part1(root))
	fmt.Fprintf(os.Stdout, "%d\n", part2(root))
}
