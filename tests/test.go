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

var list *Entity
func main() {
	x := new(Vec2)
	x.x = 16
	x.y = 6

	for i := range 100 {
		UpdateEntities()
	}
}

func SpawnEntity() {
	new_e := new(Entity)
	new_e.next = list
	new_e.pos.x = 0
	new_e.pos.y = 0
	return new_e
}

func UpdateEntities() {
	for e := list; e != nil; e = e.next {
		e.x = e.x + 1
		e.y = e.y + 2
	}
}

