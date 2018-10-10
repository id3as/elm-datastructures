module QueueTest exposing (..)

import Datastructures.Queue as Queue
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Test exposing (..)


queue_tests : Test
queue_tests =
    describe "Queue Tests"
        [ fuzz (list int) "order maintained" order_maintained
        , fuzz2 (list int) int "order maintained enqueue" order_maintained_enqueue
        , fuzz (list int) "order maintained dequeue" order_maintained_dequeue
        , fuzz (list bool) "operations equivalence" operations_equivalence
        ]


order_maintained : List a -> Expectation
order_maintained list =
    Queue.toList (Queue.fromList (list)) |> Expect.equal list


order_maintained_enqueue : List a -> a -> Expectation
order_maintained_enqueue list item =
    let
        queue =
            Queue.fromList list

        enqueued =
            Queue.enqueue item queue
    in
        Queue.toList enqueued |> Expect.equal (item :: list)


order_maintained_dequeue : List a -> Expectation
order_maintained_dequeue list =
    let
        queue =
            Queue.fromList list

        ( _, dequeued ) =
            Queue.dequeue queue

        list2 =
            List.take (List.length list - 1) list
    in
        Queue.toList dequeued |> Expect.equal list2


operations_equivalence : List Bool -> Expectation
operations_equivalence ops =
    let
        ( _, _, queueSteps ) =
            List.foldr
                (\op ( q, i, steps ) ->
                    case op of
                        True ->
                            let
                                q2 =
                                    Queue.enqueue i q
                            in
                                ( q2, i + 1, (q2 :: steps) )

                        False ->
                            let
                                ( _, q2 ) =
                                    Queue.dequeue q
                            in
                                ( q2, i, (q2 :: steps) )
                )
                ( Queue.init, 0, [] )
                ops

        ( _, _, listSteps ) =
            List.foldr
                (\op ( q, i, steps ) ->
                    case op of
                        True ->
                            ( i :: q, i + 1, ((i :: q) :: steps) )

                        False ->
                            let
                                q2 =
                                    List.take ((List.length q) - 1) q
                            in
                                ( q2, i, (q2 :: steps) )
                )
                ( [], 0, [] )
                ops
    in
        List.map Queue.toList queueSteps |> Expect.equal listSteps


correct_length : List Bool -> Expectation
correct_length ops =
    let
        ( _, _, queueSteps ) =
            List.foldr
                (\op ( q, i, steps ) ->
                    case op of
                        True ->
                            let
                                q2 =
                                    Queue.enqueue i q

                                l =
                                    Queue.length q2
                            in
                                ( q2, i + 1, (( q2, l ) :: steps) )

                        False ->
                            let
                                ( _, q2 ) =
                                    Queue.dequeue q

                                l =
                                    Queue.length q2
                            in
                                ( q2, i, (( q2, l ) :: steps) )
                )
                ( Queue.init, 0, [] )
                ops

        ( _, _, listSteps ) =
            List.foldr
                (\op ( l, i, steps ) ->
                    case op of
                        True ->
                            let
                                l2 =
                                    i :: l
                            in
                                ( i :: l, i + 1, (( l2, List.length l2 ) :: steps) )

                        False ->
                            let
                                l2 =
                                    List.take ((List.length l) - 1) l
                            in
                                ( l2, i, (( l2, List.length l2 ) :: steps) )
                )
                ( [], 0, [] )
                ops
    in
        List.map (\( q, l ) -> ( Queue.toList q, l )) queueSteps |> Expect.equal listSteps
