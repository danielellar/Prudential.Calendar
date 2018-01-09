namespace Prudential.Calendar

module PrudentialYear =
    type Years = Years of int

    let create value = 
        let min = 1980
        let max = 2038
        if value < min || value > max
        then None
        else Some (Years value)
        
    let value (Years e) = e

module PrudentialMonth =
    type Months = Months of int

    let create value = 
        let min = 1
        let max = 12
        if value < min || value > max
        then None
        else Some (Months value)

    let value (Months e) = e

module Date =
    type CalendarRecord = { Years : PrudentialYear.Years; Months : PrudentialMonth.Months } 

    let inline (~+) (x : CalendarRecord, y : CalendarRecord) =
        let monthsSum = PrudentialMonth.value x.Months + PrudentialMonth.value y.Months
        let months = monthsSum % 12
        let years = PrudentialYear.value x.Years + PrudentialYear.value y.Years + (if months > 12 then 1 else 0)
        let resultMonths = PrudentialMonth.create months
        let resultYears = PrudentialYear.create years

        if resultMonths.IsSome && resultYears.IsSome 
        then { Years = resultYears.Value; Months = resultMonths.Value } 
        else raise (System.ArgumentException("Unable to add these two Dates"))

    let inline (~-) (x : CalendarRecord, y : CalendarRecord) =
        let months = PrudentialMonth.value x.Months - PrudentialMonth.value y.Months
        let isMonthsNegative = months < 0
        let years = PrudentialYear.value x.Years - PrudentialYear.value y.Years - (if isMonthsNegative then 1 else 0)
        let resultMonths = PrudentialMonth.create months
        let resultYears = PrudentialYear.create years

        if resultMonths.IsSome && resultYears.IsSome 
        then { Years = resultYears.Value; Months = resultMonths.Value } 
        else raise (System.ArgumentException("Unable to subtract these two Dates"))

    let inline (--) (x : CalendarRecord, y : PrudentialMonth.Months) =
        let months = PrudentialMonth.value x.Months - PrudentialMonth.value y
        let isMonthsNegative = months < 0
        let years = PrudentialYear.value x.Years - (if isMonthsNegative then 1 else 0)
        let resultMonths = PrudentialMonth.create months
        let resultYears = PrudentialYear.create years

        if resultMonths.IsSome && resultYears.IsSome 
        then { Years = resultYears.Value; Months = resultMonths.Value } 
        else raise (System.ArgumentException("Unable to subtract these two Dates"))

    let inline (++) (x : CalendarRecord, y : PrudentialMonth.Months) =
        let monthsSum = PrudentialMonth.value x.Months + PrudentialMonth.value y
        let months = monthsSum % 12
        let years = PrudentialYear.value x.Years + (if months > 12 then 1 else 0)
        let resultMonths = PrudentialMonth.create months
        let resultYears = PrudentialYear.create years

        if resultMonths.IsSome && resultYears.IsSome 
        then { Years = resultYears.Value; Months = resultMonths.Value } 
        else raise (System.ArgumentException("Unable to add these two Dates"))

    