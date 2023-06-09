Program pphasepot 
    implicit real*8 (a-h,o-z)
    real*8 ppoint(100)
    real*8,dimension(100) ::ppot,ppotmm,ppotmp,ppotpm,ppotpp
    integer j,inn,i,k
    logical heform,sing,trip,coup,endep
    integer end_of_file
    character*4 label
    character(len=100) line
    common /cpot/   v(6),xmev,ymev
    common /cstate/ j,heform,sing,trip,coup,endep,label
    common /cnn/ inn
    real*8 :: convert=197.32705d0
    external n3lo500new
        
    inn=3
    j=3
    xmev=100.0d0
    ymev=100.0d0   
    heform=.false.
    sing=.true.
    trip=.true.
    coup=.true.
    call n3lo500new

    open (unit=10,file='pphasepot_input')
    open (unit=11,file='matrixnum')
    do
        read(10, '(A)', iostat=end_of_file) line
        if(end_of_file /= 0) exit
        write(11, '(A)') trim(line)
    end do
    close(10)
    close(11)
    open (unit=11,file='matrixnum')
    do i=1,7
        read(11,*)
    end do
    do i=1,100
        read(11,*) ppoint(i)
    end do
    do
        read(11, '(A)', iostat=end_of_file) line
        if(end_of_file /= 0) exit
    end do
    backspace(11)
    ppoint=ppoint*convert
1000 format(100f20.14) 
2000 format(100f20.14,100f20.14)
     do inn=1,3
        j=0
        ! 1S0
        write(11,*) "J:"
        write(11,*) j
        write(11,*) "Prty:"
        write(11,*) 1
        write(11,*) "S:"
        write(11,*) 0
        write(11,*) "Tz:"
        write(11,*) inn-2
        write(11,*) "Ndim:"
        write(11,*) 100
        write(11,*) "V:"
        do i=1,100
            do k=1,100
                xmev=ppoint(i)
                ymev=ppoint(k)
                call n3lo500new
                v=convert**3*v
                ppot(k)=v(1)
            end do
            write(11,1000) ppot
        end do
        ! 3P0
        write(11,*) "J:"
        write(11,*) j
        write(11,*) "Prty:"
        write(11,*) -1
        write(11,*) "S:"
        write(11,*) 1
        write(11,*) "Tz:"
        write(11,*) inn-2
        write(11,*) "Ndim:"
        write(11,*) 100
        write(11,*) "V:"
        do i=1,100
            do k=1,100
                xmev=ppoint(i)
                ymev=ppoint(k)
                call n3lo500new
                v=convert**3*v
                ppot(k)=v(3)
            end do
            write(11,1000) ppot
        end do
        if((inn .eq.1) .or. (inn .eq.3))then
            do j=1,8
                if(mod(j,2).eq.0)then
                ! single chanel    
                    write(11,*) "J:"
                    write(11,*) j
                    write(11,*) "Prty:"
                    write(11,*) 1
                    write(11,*) "S:"
                    write(11,*) 0
                    write(11,*) "Tz:"
                    write(11,*) inn-2
                    write(11,*) "Ndim:"
                    write(11,*) 100
                    write(11,*) "V:"
                    do i=1,100
                        do k=1,100
                            xmev=ppoint(i)
                            ymev=ppoint(k)
                            call n3lo500new
                            v=convert**3*v
                            ppot(k)=v(1)
                        end do
                        write(11,1000) ppot
                    end do
                ! coupled chanel
                    write(11,*) "J:"
                    write(11,*) j
                    write(11,*) "Prty:"
                    write(11,*) -1
                    write(11,*) "S:"
                    write(11,*) 1
                    write(11,*) "Tz:"
                    write(11,*) inn-2
                    write(11,*) "Ndim:"
                    write(11,*) 200
                    write(11,*) "V:"
                    do i=1,100
                        do k=1,100
                            xmev=ppoint(i)
                            ymev=ppoint(k)
                            call n3lo500new
                            v=convert**3*v
                            ppotmm(k)=v(4)
                            ppotmp(k)=v(6)
                        end do
                        write(11,2000) ppotmm,ppotmp
                    end do
                    do i=1,100
                        do k=1,100
                            xmev=ppoint(i)
                            ymev=ppoint(k)
                            call n3lo500new
                            v=convert**3*v
                            ppotpm(k)=v(5)
                            ppotpp(k)=v(3)
                        end do
                        write(11,2000) ppotpm,ppotpp
                    end do
                else
                ! single chanel    
                    write(11,*) "J:"
                    write(11,*) j
                    write(11,*) "Prty:"
                    write(11,*) -1
                    write(11,*) "S:"
                    write(11,*) 1
                    write(11,*) "Tz:"
                    write(11,*) inn-2
                    write(11,*) "Ndim:"
                    write(11,*) 100
                    write(11,*) "V:"
                    do i=1,100
                        do k=1,100
                            xmev=ppoint(i)
                            ymev=ppoint(k)
                            call n3lo500new
                            v=convert**3*v
                            ppot(k)=v(2)
                        end do
                        write(11,1000) ppot
                    end do
                end if
            end do
            ! nn or pp finished
        else
            ! np begin
            do j=1,8
                ! single chanel  S=0  
                write(11,*) "J:"
                write(11,*) j
                write(11,*) "Prty:"
                if(mod(j,2).eq.0)then
                    write(11,*) 1
                else
                    write(11,*) -1
                end if
                write(11,*) "S:"
                write(11,*) 0
                write(11,*) "Tz:"
                write(11,*) inn-2
                write(11,*) "Ndim:"
                write(11,*) 100
                write(11,*) "V:"
                do i=1,100
                    do k=1,100
                        xmev=ppoint(i)
                        ymev=ppoint(k)
                        call n3lo500new
                        v=convert**3*v
                        ppot(k)=v(1)
                    end do
                    write(11,1000) ppot
                end do
                ! single chanel S=1
                write(11,*) "J:"
                write(11,*) j
                write(11,*) "Prty:"
                if(mod(j,2).eq.0)then
                    write(11,*) 1
                else
                    write(11,*) -1
                end if
                write(11,*) "S:"
                write(11,*) 1
                write(11,*) "Tz:"
                write(11,*) inn-2
                write(11,*) "Ndim:"
                write(11,*) 100
                write(11,*) "V:"
                do i=1,100
                    do k=1,100
                        xmev=ppoint(i)
                        ymev=ppoint(k)
                        call n3lo500new
                        v=convert**3*v
                        ppot(k)=v(2)
                    end do
                    write(11,1000) ppot
                end do
            ! coupled chanel
                write(11,*) "J:"
                write(11,*) j
                write(11,*) "Prty:"
                if(mod(j,2).eq.0)then
                    write(11,*) -1
                else
                    write(11,*) 1
                end if
                write(11,*) "S:"
                write(11,*) 1
                write(11,*) "Tz:"
                write(11,*) inn-2
                write(11,*) "Ndim:"
                write(11,*) 200
                write(11,*) "V:"
                do i=1,100
                    do k=1,100
                        xmev=ppoint(i)
                        ymev=ppoint(k)
                        call n3lo500new
                        v=convert**3*v
                        ppotmm(k)=v(4)
                        ppotmp(k)=v(6)
                    end do
                    write(11,2000) ppotmm,ppotmp
                end do
                do i=1,100
                    do k=1,100
                        xmev=ppoint(i)
                        ymev=ppoint(k)
                        call n3lo500new
                        v=convert**3*v
                        ppotpm(k)=v(5)
                        ppotpp(k)=v(3)
                    end do
                    write(11,2000) ppotpm,ppotpp
                end do
            end do   
        end if
        ! np end
    end do
    close(11)
    stop
end program
