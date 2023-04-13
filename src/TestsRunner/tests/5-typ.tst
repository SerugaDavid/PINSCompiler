!compiler_flags: --dump TYP --exec TYP



!name: Program 1
!code:
typ x: y;
typ y: x
!failure:
99
!end

!name: Program 2
!code:
typ x: y;
typ y: z;
typ z: n;
var m: integer;
typ n: y
!failure:
99
!end

!name: Program 3
!code:
fun f(x: integer, y: string): string = x + y
!failure:
99
!end

!name: Program 4
!code:
fun f(x: integer, y: string): logical = x + y
!failure:
99
!end

!name: Program 5
!code:
fun f(x: integer, y: string): arr[10] arr[20] integer = x + y
!failure:
99
!end

!name: Program 6
!code:
typ x: z;
fun f(x: z, y: string): integer = x + y;
typ z: string
!failure:
99
!end

!name: Program 7
!code:
typ x: z;
fun f(x: z, y: string): integer = x + y;
typ z: n;
typ n: x
!failure:
99
!end

!name: Program 8
!code:
fun f(x: string, y: string): string = x + y
!failure:
99
!end

!name: Program 9
!code:
fun f(x: string, y: string): string = x * y
!failure:
99
!end

!name: Program 10
!code:
fun f(x: string, y: string): string = x / y
!failure:
99
!end

!name: Program 11
!code:
fun f(x: string, y: string): string = x % y
!failure:
99
!end

!name: Program 12
!code:
fun f(x: string, y: logical): string = x + y
!failure:
99
!end

!name: Program 12
!code:
fun f(x: logical, y: logical): string = x + y
!failure:
99
!end

!name: Program 13
!code:
fun f(x: logical, y: logical): string = x * y
!failure:
99
!end

!name: Program 14
!code:
fun f(x: logical, y: logical): string = x / y
!failure:
99
!end

!name: Program 15
!code:
fun f(x: logical, y: logical): string = x == y
!failure:
99
!end

!name: Program 16
!code:
fun f(x: logical, y: logical): string = x >= y
!failure:
99
!end

!name: Program 17
!code:
fun f(x: logical, y: logical): string = x <= y
!failure:
99
!end

!name: Program 18
!code:
fun f(x: logical, y: logical): string = x < y
!failure:
99
!end

!name: Program 19
!code:
fun f(x: logical, y: logical): string = x > y
!failure:
99
!end

!name: Program 20
!code:
fun f(x: logical, y: logical): string = x != y
!failure:
99
!end

!name: Program 21
!code:
fun f(x: logical, y: string): logical = x != y
!failure:
99
!end

!name: Program 22
!code:
fun f(x: logical, y: integer): logical = x != y
!failure:
99
!end

!name: Program 23
!code:
fun f(x: integer, y: integer): integer = x != y
!failure:
99
!end

!name: Program 24
!code:
fun f(x: integer, y: integer): integer = !x
!failure:
99
!end

!name: Program 25
!code:
fun f(x: integer, y: integer): logical = +x-y
!failure:
99
!end

!name: Program 26
!code:
fun f(x: logical, y: logical): logical = +x-y
!failure:
99
!end

!name: Program 27
!code:
fun f(x: logical, y: integer): logical = x&y
!failure:
99
!end

!name: Program 28
!code:
fun f(x: logical, y: logical): string = x&y
!failure:
99
!end

!name: Program 29
!code:
fun f(x: logical, y: logical): string = x|y
!failure:
99
!end

!name: Program 30
!code:
fun f(x: logical, y: logical): string = !x|y-y+x*y
!failure:
99
!end

!name: Program 31
!code:
typ k: integer;
fun f(x: logical, y: integer, z: arr[10] k): integer = y[10]
!failure:
99
!end

!name: Program 32
!code:
typ k: integer;
fun f(x: logical, y: integer, z: arr[10] k): integer = z[10] + x
!failure:
99
!end!

!name: Program 33
!code:
fun f(x:integer, y:integer): integer = g(x);
fun g(x:integer, y:integer): integer = f(x)
!failure:
99
!end

!name: Program 34
!code:
fun f(x:integer, y:integer): integer = g(x, y);
fun g(x:integer, y:string): integer = f(x,y)
!failure:
99
!end

!name: Program 35
!code:
fun f(x:integer, y:integer): logical = g(x, y);
fun g(x:integer, y:integer): integer = f(x,y)
!failure:
99
!end

!name: Program 36
!code:
fun f(x:integer, y:integer): integer = g(x, y);
fun g(x:integer, y:integer): integer = f(x,y);
fun h(z: integer, y:string): logical = f(z,z)
!failure:
99
!end

!name: Program 37
!code:
fun f(x: integer, y: integer): string = {x = y}
!failure:
99
!end

!name: Program 38
!code:
fun f(x: integer, y: logical): integer = {x = y}
!failure:
99
!end

!name: Program 39
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = { while x>10: k }
!failure:
99
!end

!name: Program 40
!code:
fun f(x: string, y: string): string = x / y
!failure:
99
!end

!name: Program 41
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({ while k : k }, x)
!failure:
99
!end

!name: Program 42
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({ while z : +z }, x)
!failure:
99
!end

!name: Program 43
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = (x, x+x+x-x, k)
!failure:
99
!end

!name: Program 44
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({if x==k then x},x)
!failure:
99
!end

!name: Program 45
!code:
fun f(x: string, y: string): string = x * y
!failure:
99
!end

!name: Program 46
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({if z==z then x else f(x,y,z)},x)
!failure:
99
!end

!name: Program 47
!code:
fun f(x: string, y: string): string = x % y
!failure:
99
!end

!name: Program 48
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({for y=x,x,x:x},x)
!failure:
99
!end

!name: Program 49
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({for x=z,x,x:x},x)
!failure:
99
!end

!name: Program 50
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({for x=x,k,x:x},x)
!failure:
99
!end

!name: Program 51
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({for x=x,x,y:x},x)
!failure:
99
!end

!name: Program 52
!code:
fun f(x: integer, y: logical, z: logical, k: string): integer = ({for x=x,x,x:x==x&k},x)
!failure:
99
!end

!name: Program 53
!code:
fun f(x: integer, y:integer, z: string): string = !z+x+x+x+x
!failure:
99
!end

!name: Program 54
!code:
fun f(x: integer, y:integer, z: string): string = z+z
!failure:
99
!end

!name: Program 55
!code:
fun f(x: string, y: string): string = x / y
!failure:
99
!end

!name: Program 56
!code:
fun f(x: integer, y:integer, z: string): string = x {
where fun g(x: integer, y: integer, z:string): string = z
}
!failure:
99
!end

!name: Program 57
!code:
fun f(x: integer, y:integer, z: string): string = g(x, y, z) {
where fun g(x: integer, y: integer, z:string): string = y
}
!failure:
99
!end

!name: Program 58
!code:
fun f(x: integer, y:integer, z: string): string = g(x, y, z) {
where fun g(x: integer, y: integer, z:string): string = (x, y, x+x+x+x+x&x,z)
}
!failure:
99
!end

!name: Program 59
!code:
fun f(x: integer, y:integer, z: string): string = g(x, y, z) {
where fun g(x: integer, y: integer, z:string): string = (x, y, x+x+x+x+x,z);
fun xyz(x: integer, y:integer):integer = f(x,y)
}
!failure:
99
!end

!name: Program 60
!code:
typ x: arr[10] integer;
fun f(x: x, y: integer): arr[9] integer = x
!failure:
99
!end