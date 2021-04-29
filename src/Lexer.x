{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]

tokens :-

  $white+                    ;
  \(                         { \_ -> LeftT   }
  \)                         { \_ -> RightT  }
  \.                         { \_ -> DotT    }
  \\                         { \_ -> LambdaT }
  $alpha [$alpha $digit \']* { \s -> VarT s  }

{
data Token = VarT String
           | LeftT
           | RightT
           | DotT
           | LambdaT
           deriving (Show, Eq)
}

