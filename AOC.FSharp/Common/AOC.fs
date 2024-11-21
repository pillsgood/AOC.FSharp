namespace Pillsgood.AdventOfCode

open Pillsgood.AdventOfCode
open Pillsgood.AdventOfCode.Common
open Splat

module Answer =
    let submit (answer: 'a) =
        let svc = Locator.Current.GetRequiredService<IAnswerAssertion>()
        svc.Submit answer

module Input =
    let fetch<'T> =
        let svc = Locator.Current.GetRequiredService<IPuzzleInputService>()
        svc.Get<'T>()
