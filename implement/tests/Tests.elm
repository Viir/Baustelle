module Tests exposing (..)

import Expect
import Scenario
import Test exposing (..)


all : Test
all =
    describe "Scenario Physics"
        [ describe "Beam Expansion Velocity"
            [ test "Is zero if joints not moving." <|
                \() ->
                    Expect.equal 0
                        (Scenario.getExpansionVelocityFromPairOfJoints
                            { location = ( 0, 0 )
                            , velocity = ( 0, 0 )
                            }
                            { location = ( 0, 4 )
                            , velocity = ( 0, 0 )
                            }
                        )
            , test "Is zero if velocities are equal." <|
                \() ->
                    Expect.equal 0
                        (Scenario.getExpansionVelocityFromPairOfJoints
                            { location = ( 0, 0 )
                            , velocity = ( 0, 3 )
                            }
                            { location = ( 0, 4 )
                            , velocity = ( 0, 3 )
                            }
                        )
            , test "Is zero if velocity is perpendicular to extend." <|
                \() ->
                    Expect.equal 0
                        (Scenario.getExpansionVelocityFromPairOfJoints
                            { location = ( 0, 0 )
                            , velocity = ( 3, 0 )
                            }
                            { location = ( 0, 4 )
                            , velocity = ( 0, 0 )
                            }
                        )
            , test "Sample case of expansion by 3." <|
                \() ->
                    Expect.equal 3
                        (Scenario.getExpansionVelocityFromPairOfJoints
                            { location = ( 0, 0 )
                            , velocity = ( 3, -1 )
                            }
                            { location = ( 0, 4 )
                            , velocity = ( -1, 2 )
                            }
                        )
            , test "Sample case of compression by 3." <|
                \() ->
                    Expect.equal -3
                        (Scenario.getExpansionVelocityFromPairOfJoints
                            { location = ( 0, 0 )
                            , velocity = ( 3, 2 )
                            }
                            { location = ( 0, 4 )
                            , velocity = ( 7, -1 )
                            }
                        )
            , test "Sample case of expansion of diagonal beam." <|
                \() ->
                    Expect.lessThan 1.0e-8
                        ((Scenario.getExpansionVelocityFromPairOfJoints
                            { location = ( 0, 0 )
                            , velocity = ( 1, 1 )
                            }
                            { location = ( 4, 4 )
                            , velocity = ( 1, 3 )
                            }
                            - 2
                            ^ 0.5
                         )
                            |> abs
                        )
            ]
        ]
