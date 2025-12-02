namespace Pillsgood.AdventOfCode

open Pillsgood.AdventOfCode
open Pillsgood.AdventOfCode.Common

module Answer =
    let submit (answer: 'a) =
        let svc = Locator.GetRequiredService<IAnswerAssertion>()
        svc.Submit(string answer)

module Input =
    let fetch<'T> =
        let svc = Locator.GetRequiredService<IPuzzleInputService>()
        svc.Get<'T>()
