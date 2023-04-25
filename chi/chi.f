      program chi
        implicit none
        character(len=:),allocatable :: phasename(:)
        real*8,allocatable :: phasetheory(:,:),phaseexp(:,:)
        real*8 :: chitemp,sum
        character(len=20) :: buffer
        integer ::phasenum,elabnum,i,j
        open(unit=10,file='chiset.d')
        read(10,"(a8,2x,i2)") buffer,phasenum
        read(10,"(a7,2x,i2)") buffer,elabnum
        allocate(character(len=3):: phasename(phasenum))
        allocate(phasetheory(elabnum,phasenum+1))
        allocate(phaseexp(elabnum,phasenum+1))
        read(10,*) buffer
        do i=1,phasenum
            read(10,"(a3)") phasename(i)
        end do
        close(10)
        open(unit=11,file='phasetheory.d')
        do i=1,elabnum
            read(11,*) phasetheory(i,:)
        end do
        close(11)
        open(unit=12,file='phaseexp.d')
        do i=1,elabnum
            read(12,*) phaseexp(i,:)
        end do
        close(12)
        sum=0.0d0
        open(unit=13,file='chi_result.d')
        do i=1,phasenum
            chitemp=0.0d0
            do j=1,elabnum
                chitemp=(phaseexp(j,i+1)-phasetheory(j,i+1))**2+chitemp
            end do
            chitemp=chitemp/dble(elabnum)
            write(13,"(a3,2x,f10.3)") phasename(i),chitemp
            sum=sum+chitemp
        end do
        sum=sum/dble(phasenum)
        sum=dsqrt(sum)
        write(13,*) sum
        close(13)
        stop
        end 
