let Self = ./Type.dhall

let Typeclasses = ../Deps/Typeclasses.dhall

let Order = Typeclasses.Classes.Ordering.Order.Type

let order
    : Self -> Self -> Order
    = \(left : Self) ->
      \(right : Self) ->
        merge
          { A =
              merge
                { A = Order.Equal
                , B = Order.Less
                , C = Order.Less
                , D = Order.Less
                , E = Order.Less
                , F = Order.Less
                , G = Order.Less
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , B =
              merge
                { A = Order.Greater
                , B = Order.Equal
                , C = Order.Less
                , D = Order.Less
                , E = Order.Less
                , F = Order.Less
                , G = Order.Less
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , C =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Equal
                , D = Order.Less
                , E = Order.Less
                , F = Order.Less
                , G = Order.Less
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , D =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Equal
                , E = Order.Less
                , F = Order.Less
                , G = Order.Less
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , E =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Equal
                , F = Order.Less
                , G = Order.Less
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , F =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Equal
                , G = Order.Less
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , G =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Equal
                , H = Order.Less
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , H =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Equal
                , I = Order.Less
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , I =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Equal
                , J = Order.Less
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , J =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Equal
                , K = Order.Less
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , K =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Equal
                , L = Order.Less
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , L =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Equal
                , M = Order.Less
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , M =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Equal
                , N = Order.Less
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , N =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Equal
                , O = Order.Less
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , O =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Equal
                , P = Order.Less
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , P =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Equal
                , Q = Order.Less
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , Q =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Equal
                , R = Order.Less
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , R =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Equal
                , S = Order.Less
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , S =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Equal
                , T = Order.Less
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , T =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Equal
                , U = Order.Less
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , U =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Greater
                , U = Order.Equal
                , V = Order.Less
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , V =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Greater
                , U = Order.Greater
                , V = Order.Equal
                , W = Order.Less
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , W =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Greater
                , U = Order.Greater
                , V = Order.Greater
                , W = Order.Equal
                , X = Order.Less
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , X =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Greater
                , U = Order.Greater
                , V = Order.Greater
                , W = Order.Greater
                , X = Order.Equal
                , Y = Order.Less
                , Z = Order.Less
                }
                right
          , Y =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Greater
                , U = Order.Greater
                , V = Order.Greater
                , W = Order.Greater
                , X = Order.Greater
                , Y = Order.Equal
                , Z = Order.Less
                }
                right
          , Z =
              merge
                { A = Order.Greater
                , B = Order.Greater
                , C = Order.Greater
                , D = Order.Greater
                , E = Order.Greater
                , F = Order.Greater
                , G = Order.Greater
                , H = Order.Greater
                , I = Order.Greater
                , J = Order.Greater
                , K = Order.Greater
                , L = Order.Greater
                , M = Order.Greater
                , N = Order.Greater
                , O = Order.Greater
                , P = Order.Greater
                , Q = Order.Greater
                , R = Order.Greater
                , S = Order.Greater
                , T = Order.Greater
                , U = Order.Greater
                , V = Order.Greater
                , W = Order.Greater
                , X = Order.Greater
                , Y = Order.Greater
                , Z = Order.Equal
                }
                right
          }
          left

in  { order } : Typeclasses.Classes.Ordering.Type Self
