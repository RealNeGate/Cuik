package main

type Vec2 struct {
	x, y int
}

type Entity struct {
	next *Entity
	pos   Vec2
}

func MaxArray(x, y []float64) {
	for i, c := range x {
		if y[i] > c {
			x[i] = y[i]
		}
	}
}

func MaxDist(list *Entity) {
}

func main() {
	x := new(Vec2)
	x.x = 16
	x.y = 6
}


