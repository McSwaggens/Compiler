struct Foo:
	a : int
	b : int

Test(x: int, y: int) -> int:
	z := x + y
	return z


Foo():
	Test(1, 2)

