! encoder/decoder for caesar, atbash, and a1z26, combined of before, vigenere, and nihilist limited transposition ciphers
program ci
implicit none
    character function cipher()
        character(len=100) :: entered, message, key
        character(len=3) :: status, answer
        print *, 'What cipher do you prefer?'
        read *, entered
        print *, 'What is your message?'
        read *, message
        print *, 'Is your message encrypted?'
        read *, status
        print *, 'If your message has a key, please type it. Otherwise write no'
        read *, key
        if to_lower(entered) == 'caesar' then
            call caesCi(message, status)
            print *, 'The message is: ', message
        else if to_lower(entered) == 'atbash' then
            call atbaCi(message, status)
            print *, 'The message is: ', message
        else if to_lower(entered) == 'a1z26' then
            call a1z26(message, status)
            print *, 'The message is: ', message
        else if to_lower(entered) == 'combined' then
            call combCi(message, status)
            print *, 'The message is: ', message
        else if to_lower(entered) == 'vigenere' then
            call vigeCi(message, status, key)
            print *, 'The message is: ', message
        else if to_lower(entered) == 'nihilist limited transposition' then
            call niCiLi(message, status)
            
        print *, 'Do you want to encrypt a message'
        read *, answer
        
        if answer == 'yes' then
            cipher()
        else
            print *, 'Goodbye'
    end function cipher
    
    cipher()
    
contains
    subroutine caesCi(message, status)
        character(len=100), intent(in,out) :: message
        character(len=3), intent(in) :: status
        character(len=100) :: temp
        integer :: i, x
        integer, dimension(26) :: normal, caesar
        normal = (/a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,x,y,z/)
        caesar = (/d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,x,y,z,a,b,c/)
        
        if to_lower(status) == 'yes' then ! encoded -> decoded
            do i = 1, len(message)
                x = findloc(normal, message[i])
                temp = temp//caesar(x)
        else ! decoded -> encoded
            do i = 1, len(message)
                x = findloc(caesar, message[i])
                temp = temp//normal(x)
        message = temp
    end subroutine caesCi
    
    subroutine atbaCi(message, status)
        character(len=100), intent(in,out) :: message
        character(len=3), intent(in) :: status
        character(len=100) :: temp
        integer :: i, x
        integer, dimension(26) :: normal, atbash
        normal = (/a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,x,y,z/)
        atbash = (/z,y,x,v,u,t,s,r,q,p,o,n,m,l,k,j,i,h,g,f,e,d,c,b,a/)
        
        if to_lower(status) == 'yes' then ! encoded -> decoded
            do i = 1, len(message)
                x = findloc(normal, message[i])
                temp = temp//atbash(x)
        else ! decoded -> encoded
            do i = 1, len(message)
                x = findloc(atbash, message[i])
                temp = temp//normal(x)
        message = temp
    end subroutine atbaCi
    
    subroutine a1z26(message, status)
        character(len=100), intent(in,out) :: message
        character(len=3), intent(in) :: status
        character(len=100) :: temp
        logical :: passed
        integer :: i, x
        integer, dimension(26) :: normal
        normal = (/a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z/)
        passed = False
        
        if to_lower(status) == 'yes' then ! encoded -> decoded
            do i = 1, len(message)
                if message[i] /= '-' .AND. passed == False then
                    if message[i+1] == '-' .OR. message[i] == ' ' then
                        temp = temp//normal(message[i])
                    else
                        temp = temp//normal(message[i:i+1])
                        passed = True
                else
                    passed = False
        else ! decoded -> encoded
            do i = 1, len(message)
                if message[i] == ' ' then
                    temp = temp//' '
                else
                    x = findloc(normal, message[i])
                    temp = temp//x//'-'
        message = temp
    end subroutine a1z26
    
    subroutine combCi(message, status) ! using the A1Z26 cipher, then flipping the letters with the Atbash cipher, 
                                       ! and finally by using the Caesar cipher
        character(len=100), intent(in,out) :: message
        character(len=3), intent(in) :: status
        character(len=100) :: temp
        logical :: passed
        integer :: i, x
        integer, dimension(26) :: normal
        normal = (/w,v,u,t,s,r,q,p,o,n,m,l,k,j,i,h,g,f,e,d,c,b,a,z,y,x/)
        
        if to_lower(status) == 'yes' then ! encoded -> decoded
            do i = 1, len(message)
                if message[i] /= '-' .AND. passed == False then
                    if message[i+1] == '-' .OR. message[i] == ' ' then
                        temp = temp//normal(message[i])
                    else
                        temp = temp//normal(message[i:i+1])
                        passed = True
                else
                    passed = False
        else ! decoded -> encoded
            do i = 1, len(message)
                if message[i] == ' ' then
                    temp = temp//' '
                else
                    x = findloc(normal, message[i])
                    temp = temp//x + '-'
        message = temp
    end subroutine combCi
    
    subroutine vigeCi(message, status, key) 
        character(len=100), intent(in,out) :: message
        character(len=3), intent(in) :: status
        character(len=100), intent(in) :: key
        character(len=100) :: temp, keyTemp
        integer :: i, x, y, clock, added
        integer, dimension(26) :: normal
        clock = 0
        normal = (/a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z/)
        
        do i = 1, len(message)
            clock += 1
            if clock > len(key) then
                clock = 1
                keyTemp = keyTemp//key[clock]
            else
                keyTemp = keyTemp//key[clock]
                
        if to_lower(status) == 'yes' then ! encoded -> decoded
            do i = 1, len(message)
                x = findloc(normal, message[i])
                y = findloc(normal, keyTemp[i])
                added = x + y - 1
                temp = temp//normal[added]
        else ! decoded -> encoded
            do i = 1, len(message)
                x = findloc(normal, message[i])
                y = findloc(normal, keyTemp[i])
                if x > y then
                    added = x - y + 1
                else
                    added = y - x + 1
                temp = temp//normal[added]
        message = temp
    end subroutine vigeCi
    
    subroutine niCiLi(message, status) ! assuming key is 2134 for now, gonna make a diff one for any key
        character(len=100), intent(in,out) :: message
        character(len=3), intent(in) :: status
        character(len=100) :: new, newNew
        integer :: i, clock
        clock = 0
        
        if to_lower(status) == 'yes' then ! encoded -> decoded
            new = message[5:8]//message[:4]//message[9:]
            do i = 1, len(new)
                clock += 1
                if clock == 1 then
                    newNew = newNew//new[i+1]//new[i]
                else if clock >= 3 then
                    newNew = newNew//new[i]
                    else if clock == 4 then
                        clock = 0
        else ! decoded -> encoded
            new = message[5:8]//message[:4]//message[9:]
            do i = 1, len(new)
                clock += 1
                if clock == 1 then
                    newNew = new[i+1]//new[i]
                else if clock >= 3 then
                    newNew = newNew//new[i]
                else if clock == 4 then
                    clock = 0
        message = newNew
    end subroutine niCiLi
