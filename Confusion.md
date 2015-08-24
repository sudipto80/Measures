
```fsharp 

module confusion =    
    let TP (matches : int [] [])  = 
        matches |> Array.mapi( fun i j -> matches.[i].[i]) |> Array.sum 

    //True Positive 
    let TP_for (thisOne : int) (matches : int [] []) = 
        matches.[thisOne].[thisOne]

    //False positive for 
    let FP_for (thisOne : int) (matches : int [] []) = 
        let all = [for i in 0 .. matches.Length-1 -> matches.[i].[thisOne]]
        let allSum = all |> List.sum
        allSum - (TP_for thisOne matches)

    //False negative for 
    let FN_for (thisOne : int) (matches : int [] []) = 
        let all = [for  i in 0 .. matches.[thisOne].Length - 1 -> matches.[thisOne].[i]]
        let allSum = all |> List.sum 
        allSum - matches.[thisOne].[thisOne]

  
    //True negativs. All other animals that are 
    let TN_for (thisOne : int) (matches : int [] []) = 
        let rows = [0 .. matches.Length - 1] |> List.filter (fun t -> t <> thisOne)
        let cols = [0 .. matches.[0].Length - 1 ] |> List.filter (fun t -> t <> thisOne)
        let carts = rows |> List.collect (fun x -> cols |> List.map (fun y -> x, y))
        carts |> List.map (fun t -> matches.[fst t].[snd t]) |> List.sum

    //True Positive Rate and Sensitivity are same thing
    let TPR (thisOne : int) (matches : int [] []) = 
       float (TP_for thisOne matches) / float( (TP_for thisOne matches + FN_for thisOne matches))
   
    //Total Positives
    let P (thisOne : int) (matches : int [] []) = 
        TP_for thisOne matches + FN_for thisOne matches 
   
    //Total Negatives
    let N (thisOne : int) (matches : int [] []) = 
        FP_for thisOne matches + TN_for thisOne matches 
   
    //Specificity 
    let SPC (thisOne : int) (matches : int [] []) = 
       float ( TN_for thisOne matches )/ float ( N thisOne matches )
   
    //Precision of Positive Predictive Value
    let PPV (thisOne : int) (matches : int [] []) = 
        float (TP_for thisOne matches )/float (TP_for thisOne matches + FP_for thisOne matches)
   
    let ``Precision``(thisOne : int) (matches : int [] []) = 
        PPV thisOne matches 
    //Negative Predictive value
    let NPV (thisOne : int) (matches : int [] []) = 
        float (TN_for thisOne matches) / float ( TN_for thisOne matches + FN_for thisOne matches)
   
    //Fall out or False Position Rate
    let FPR (thisOne : int) (matches : int [] []) = 
        float (FP_for thisOne matches) / float (N thisOne matches)
   
    //False Discovery Rate 
    let FDR (thisOne : int) (matches : int [] []) = 
        FP_for thisOne matches 
   
    //False Negative Rate or "miss" rate
    let FNR (thisOne : int) (matches : int [] []) = 
        float (FN_for thisOne matches) / float( P thisOne matches )     
   
    //F1 
    let F1(thisOne : int) (matches : int [] []) = 
        2. * float (TP_for thisOne matches) /
        ( 2. * float (TP_for thisOne matches) + float( FP_for thisOne matches )+ float (FN_for thisOne matches))

    //Matthews correlation coefficient (MCC)
    let MCC (thisOne : int) (matches : int [] []) = 
       let numerator = TP_for thisOne matches * TN_for thisOne matches - 
                       FP_for thisOne matches * FN_for thisOne matches 
       let A = float (TP_for thisOne matches + FP_for thisOne matches)
       let B = float (TP_for thisOne matches + FN_for thisOne matches)
       let C = float (TN_for thisOne matches + FP_for thisOne matches)
       let D = float (TN_for thisOne matches + FN_for thisOne matches)

       let denominator = sqrt (A * B * C * D)
       float numerator / denominator
    
    //Accuracy 
    let ACC (thisOne : int) (matches : int[][] ) = 
        let numerator = float (TP_for thisOne matches + TN_for thisOne matches )
        let denominator = float (P thisOne matches + N thisOne matches)
        numerator/denominator 

    //Sensitity for a given instance 
    let ``Sensitivity for`` (thisOne : int) (matches : int [] []) = 
       TPR thisOne matches

    //Specificity for a given instance
    let ``Specificity for`` (thisOne: int)(matches: int[][])  =    
        SPC thisOne matches 

    let ``Informedness``(thisOne:int) (matches:int[][]) = 
        ``Sensitivity for`` thisOne matches + ``Specificity for`` thisOne matches - 1.  

    let ``Markedness``(thisOne:int) (matches:int[][]) = 
        Precision thisOne matches + NPV thisOne matches - 1.

    let LRPlus (thisOne : int) (matches : int [] []) = 
        float (TPR thisOne matches ) / float (FPR thisOne matches)

    let LRMinus (thisOne : int) (matches : int [] []) = 
        float (FNR thisOne matches) / float (SPC thisOne matches)
```
