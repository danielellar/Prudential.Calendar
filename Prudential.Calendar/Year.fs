module Year
    type Year = Year of int

    let create value = 
        let min = 1980
        let max = 2038
        if value < min || value > max
        then None
        else Some (Year value)

module Month
    type Month = Month of int

    let create value = 
        let min = 1
        let max = 12
        if value < min || value > max
        then None
        else Some (Month value)


