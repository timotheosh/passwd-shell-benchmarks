program count_shells
    implicit none

    ! Define constants
    integer, parameter :: MAX_SHELLS = 64
    integer, parameter :: SHELL_LEN = 64
    integer, parameter :: LINE_LEN = 256

    ! Declare variables
    character(len=LINE_LEN) :: line
    character(len=SHELL_LEN), dimension(MAX_SHELLS) :: shells
    integer, dimension(MAX_SHELLS) :: shellcnt
    integer :: numshells, io_status, colon_pos, k, i
    character(len=SHELL_LEN) :: current_shell
    logical :: found

    ! Initialize arrays and counters
    shells = ' '
    shellcnt = 0
    numshells = 0

    ! Open the password file
    open(unit=10, file='passwd', status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
        print *, "Error opening file"
        stop
    end if

    ! Read line by line until End of File (EOF)
    do
        read(10, '(A)', iostat=io_status) line
        if (io_status /= 0) exit ! Exit loop at EOF or error

        ! Find the last colon in the line
        colon_pos = scan(line, ':', back=.true.)
        if (colon_pos == 0) cycle ! Skip line if no colon is found

        ! Extract the shell path and trim surrounding spaces
        current_shell = adjustl(line(colon_pos+1:))
        
        ! Check if the shell already exists in our array
        found = .false.
        do k = 1, numshells
            if (trim(shells(k)) == trim(current_shell)) then
                shellcnt(k) = shellcnt(k) + 1
                found = .true.
                exit
            end if
        end do

        ! If it is a new shell, add it safely
        if (.not. found .and. numshells < MAX_SHELLS) then
            numshells = numshells + 1
            shells(numshells) = current_shell
            shellcnt(numshells) = 1
        end if
    end do

    ! Close the file
    close(10)

    ! Display shell tally
    do i = 1, numshells
	write(*, '(A18, ":", T25, I0)') trim(shells(i)), shellcnt(i)
    end do

end program count_shells

