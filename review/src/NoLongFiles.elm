module NoLongFiles exposing (rule)

import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Direction, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoLongFiles" 0
        |> Rule.withModuleDefinitionVisitor moduleDefinitionVisitor
        |> Rule.withDeclarationVisitor declarationVisitor
        |> Rule.withCommentsVisitor commentsVisitor
        |> Rule.withFinalModuleEvaluation finalEvaluation
        |> Rule.fromModuleRuleSchema


updateMax : Node a -> Int -> Int
updateMax node currentMax =
    max currentMax (Node.range node).end.row


moduleDefinitionVisitor : Node Module -> Int -> ( List (Rule.Error {}), Int )
moduleDefinitionVisitor node maxLine =
    ( [], updateMax node maxLine )


declarationVisitor : Node Declaration -> Direction -> Int -> ( List (Rule.Error {}), Int )
declarationVisitor node _ maxLine =
    ( [], updateMax node maxLine )


commentsVisitor : List (Node String) -> Int -> ( List (Rule.Error {}), Int )
commentsVisitor nodes maxLine =
    ( [], List.foldl updateMax maxLine nodes )


finalEvaluation : Int -> List (Rule.Error {})
finalEvaluation maxLine =
    if maxLine > 400 then
        [ Rule.error
            { message = "File is too long (" ++ String.fromInt maxLine ++ " lines)"
            , details = [ "Files should be under 400 lines to be LLM-friendly. Please split this file." ]
            }
            { start = { row = 1, column = 1 }, end = { row = 1, column = 1 } }
        ]

    else
        []
