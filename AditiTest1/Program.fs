// For more information see https://aka.ms/fsharp-console-apps
printfn "This is Aditi Thapa"

// list of salaries
let tot_sal = [75000; 48000; 120000; 190000; 300113; 92000; 36000]

// printing salaries
printfn "Your Final Salaries: %A" tot_sal

// Filtering high income salaries
let highincomesalaries = List.filter (fun salary -> salary > 100000) tot_sal
printfn "the total High-income salaries: %A" highincomesalaries

// used map function for mapping corresponding salary
let taxcalculation salary =
    match salary with
    | a when a <= 49020 -> float a * 0.15
    | a when a > 49020 && a <= 98040 -> float a * 0.205
    | a when a > 98040 && a <= 151978 -> float a * 0.26
    | a when a > 151978 && a <= 216511 -> float a * 0.29
    | a -> float a * 0.33

let tax = List.map taxcalculation tot_sal
printfn "Total number of tax on salaries: %A" tax

// Added $20,000 to salaries less than $49,020
let newsalaries = List.map (fun salary -> if salary < 49020 then salary + 20000 else salary) tot_sal
printfn "Salaries after tax calculation: %A" newsalaries

//finding Sum of salaries between $50,000 and $100,000 use List.reduce
let totalsumbetween50and100k = 
    List.filter (fun salary -> salary >= 50000 && salary <= 100000) tot_sal 
    |> List.reduce (+)
printfn "Sum of total salaries between $50,000 and $100,000: %d" totalsumbetween50and100k



// Tail Recursion
let rec sumofmultiplesof3_acc number acc =
    if number = 0 then acc
    else sumofmultiplesof3_acc (number - 3) (acc + number)

let sumofmultiplesof3 number =
    sumofmultiplesof3_acc number 0

let result = sumofmultiplesof3 27
printfn "The total Sum of all the multiples of 3 up to 27 is: %d" result
