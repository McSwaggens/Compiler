Alpha() -> int if TEST = 3 else float32:
	return 123

TEST := Foo()

Foo() -> int:
	x := Add(1, 2)
	return x

Add(a : int, b : int) -> int:
	return a + b


