try to put theese examples into "input.txt"

 example 1  ---------------------------- ---------------------------- ---------------------------- 

[basic]
print TABLE_START
let RowColor = 255

for a = 1 to 10

    print TABLE_ROW_START

    let ColumnColor = 255

    if RowColor = 255 let RowColor = 0 endif elseif let RowColor = 255 endif
    
    for b = 1 to 10
        print TABLE_CELL_START

        if (a = 1) or (b = 1)
            let PrintSize = 7
        endif elseif
            let PrintSize = 6
        endif

        if ColumnColor = 255 
            let ColumnColor = 0 
        endif elseif 
            let ColumnColor = 255 
        endif

        print [size=printsize face = "Courier" color = ColumnColor 0 RowColor] a*b

        print TABLE_CELL_END
    endf
    print TABLE_ROW_END
endf

print TABLE_END
[/basic]

 example 2  ---------------------------- ---------------------------- ---------------------------- 

[basic]
let text = "Text examlpe"
for j = 1 to 3
    for i = 1 to 7
        if j = 1 let Style = "Arial" endif
        if j = 2 let Style = "Courier" endif
        if j = 3 let Style = "Times New Roman" endif
        print [size = i face = Style] text, eoln    
    endf
    print eoln, eoln, eoln
endf
[/basic]

 example 3  ---------------------------- ---------------------------- ---------------------------- 

[basic]
let mov1 = 0 
let mov2 = 0 
let mov4 = 0 

for y = 1 to 300

    let mov1 = mov1 + 9
    let mov2 = mov1

    for x = 1 to 100
        let mov4 = mov4 + 19
        let mov2 = mov2 + 15
        if y > 10 
          print [size=0 face="Arial" color = 
                 abs(sin(3*(mov1)/255)*255+sin(2*(mov2)/255)*100-200)
                 abs(sin(mov4/(y))*255)
                 abs(sin(mov1/70)*sin(mov2/90))*300
                ] "&#9608"
        endif
    endf
    if y > 10 print eoln endif
endf

[/basic]

 example 4  ---------------------------- ---------------------------- ---------------------------- 

[basic]

let mov1 = 0 
let mov2 = 0 
let mov4 = 0 

for y = 1 to 300

    let mov1 = mov1 + 9
    let mov2 = mov1

    for x = 1 to 100
        let mov4 = mov4 + 19
        let mov2 = mov2 + 15
        if y > 250
          print [   size=0 face="Courier" color = 
                    abs(sin(2*(mov1)/255)*255+sin(2*(mov2)/255)*100-200) 
                    abs(sin(1+mov4/(y))*255) 
                    abs(sin(6*mov1/20)*sin(mov2/90))*300
                ] "&#9608"
        endif
    endf
    if y > 250 print eoln endif
endf

[/basic]

 example 5  ---------------------------- ---------------------------- ---------------------------- 

[basic]

let mov1 = 0
let mov2 = 0
let mov4 = 0

let y = 0
while y < 30
    let y = y + 1

    let mov1 = mov1 + 8
    let mov2 = mov1

    let x = 0
    while x < 80
        let x = x + 1
        let mov4 = mov4 + 7

        let mov2 = mov2 + 3
        print [size=1 face="Arial" color = 
                abs(sin(3*(mov1)/255)*100)+
                abs(sin(3*(mov2)/255)*100)+
                abs(sin(mov4/255)*100)+
                abs(sin(3*(mov1+mov2)/255)*100)

                abs(sin(6*(mov2)/74)*100)


                0

              ] "&#9608"
    endw

    print eoln
endw

[/basic]
