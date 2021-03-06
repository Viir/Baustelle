module Vector2 exposing (..)


type alias Float2 =
    ( Float, Float )


{-| `v + w`
add (1,2) (4,5) == (5,7)
-}
add : Float2 -> Float2 -> Float2
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| `v - w`
sub (3,1) (-3,8) == (6,-7)
-}
sub : Float2 -> Float2 -> Float2
sub ( x1, y1 ) ( x2, y2 ) =
    ( x1 - x2, y1 - y2 )


{-| `-v`
negate (2,-4) == (-2,4)
-}
negate : Float2 -> Float2
negate ( x, y ) =
    ( -x, -y )


{-| `a*v`
scale 3 ( 2, 3 ) =
( 6, 9 )
-}
scale : Float -> Float2 -> Float2
scale a ( x, y ) =
    ( a * x, a * y )


{-| `v/a`
divideBy 4 (12,16) == (3,4)
NaN/infinity warning: if a = 0
-}
divideBy : Float -> Float2 -> Float2
divideBy a ( x, y ) =
    ( x / a, y / a )


{-| `v dot w`
The **dot product** of two vectors. Also called **scalar product** or **inner product**.
It links the length and angle of two vectors.
`v dot w = |v|*|w|*cos(phi)`
dot (1,2) (3,2) == 1\_3 + 2\_2 == 7
-}
dot : Float2 -> Float2 -> Float
dot ( x1, y1 ) ( x2, y2 ) =
    x1 * x2 + y1 * y2


{-| The projection of `v` onto `w`.
`(v dot w)/|w| * w/|w|`
project (2,1) (4,0) == (2,0)
NaN/infinity warning: if w = 0
-}
project : Float2 -> Float2 -> Float2
project v w =
    let
        l_w =
            lengthSquared w
    in
    scale (dot v w / l_w) w


{-| The rejection of `v` onto `w`. This is always perpendicular to the projection.
`v - (project v w)`
reject (2,1) (4,0) == (0,1)
NaN/infinity warning: if w = 0
-}
reject : Float2 -> Float2 -> Float2
reject v w =
    sub v (project v w)


{-| The length of a vector. Also known as magnitude or norm.
`|v| = sqrt(v dot v)`
length (3,4) == sqrt(3^2+4^2) == 5
-}
length : Float2 -> Float
length v =
    sqrt (dot v v)


{-| The squared length. This is cheaper to calculate,
so if you only need to compare lengths you can use this instead of the length.
`|v|^2 = v dot w`
lengthSquared (3,4) == 3^2+4^2 == 25
-}
lengthSquared : Float2 -> Float
lengthSquared v =
    dot v v


{-| Normalizes a vector. This will give you a unit vector (e.g. with length 1) in the same direction as `v`.
`v/|v|`
normalize (3,4) == (3/5,4/5)
NaN warning: if v = 0
-}
normalize : Float2 -> Float2
normalize v =
    divideBy (length v) v


{-| A unit vector pointing from `v` to `w`
`(w - v)/|w - v|`
directionFromTo (5,1) (8,5) == (3/5,4/5)
NaN warning: if v = w
-}
directionFromTo : Float2 -> Float2 -> Float2
directionFromTo v w =
    normalize (sub w v)


{-| Calculates the distance from `v` to `w`.
`|v - w| = |w - v|`
distance (3,0) (0,4) == 5
-}
distance : Float2 -> Float2 -> Float
distance v w =
    length (sub v w)


{-| The squared distance. This is slightly faster.
`|v - w|^2`
distanceSquared (3,0) (0,4) == 25
-}
distanceSquared : Float2 -> Float2 -> Float
distanceSquared v w =
    lengthSquared (sub v w)


{-| The angle between two vectors. The angle is in radians.
`acos((v dot w)/(|v|*|w|))`
angle (1,0) (2,2) == pi/4 -- or 45°
NaN warning: if v = 0 or w = 0
-}
angle : Float2 -> Float2 -> Float
angle v w =
    let
        r =
            dot v w / (length v * length w)
    in
    if r >= 1 then
        0

    else
        acos r
