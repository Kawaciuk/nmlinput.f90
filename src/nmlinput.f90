module nmlinput

    use, intrinsic :: iso_fortran_env, only: stderr => error_unit

contains

    subroutine open_inputfile(file_path, file_unit, iostat_)
        character(len=*), intent(in) :: file_path
        integer, intent(out) :: file_unit
        integer, intent(out) :: iostat_
        logical :: file_exists

        inquire (file=file_path, iostat=iostat_, exist=file_exists)
        if (.not. file_exists) then
            write (stderr, '(3a)') &
                "Error: file '", trim(file_path), "' not found"
        end if
        open (action='read', file=file_path, iostat=iostat_, newunit=file_unit)
    end subroutine open_inputfile

    subroutine open_optinputfile(file_path, file_unit, file_exists, iostat_)
        character(len=*), intent(in) :: file_path
        integer, intent(out) :: file_unit
        integer, intent(out) :: iostat_
        logical, intent(out) :: file_exists

        inquire (file=file_path, exist=file_exists)
        if (file_exists) then
            open (action='read', file=file_path, iostat=iostat_, &
                newunit=file_unit)
        end if
    end subroutine open_optinputfile

    subroutine close_inputfile(file_path, file_unit, iostat_)
        character(len=*), intent(in) :: file_path
        integer, intent(out) :: file_unit
        integer, intent(out) :: iostat_
        character(len=100) :: line

        if (iostat_ /= 0) then
            write (stderr, '(3a)') &
                "Error reading file: '", trim(file_path), "'"
            backspace (file_unit)
            read (file_unit, fmt='(a)') line
            write (stderr, '(2a)') &
                'Invalid line: ', trim(line)
        endif
        close (file_unit)
    end subroutine

end module nmlinput
