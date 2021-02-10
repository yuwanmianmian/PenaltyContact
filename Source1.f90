subroutine GetLocForce(ps,p1,p2,MatProp,ps0,p10,p20,Force,IsInterpentration)
    implicit none
        double precision, intent(in) :: ps(2),p1(2),p2(2),MatProp(3),ps0(2),p10(2),p20(2)
        double precision, intent(out) :: Force(6)
        logical, intent(out) :: IsInterpentration
        double precision :: xt(2), xt0(2), xlen, xlen0, xn(2), gapn, xi, xi0, FRTOL, GAPT, XLAMBN, XLAMBT
        logical :: IsContact, IsSegment, IsFric, IsSlide
    
        Force = 0.d0
        !Calculate unit normals and tangential vectors
        !Current time step
        xt(1) = p2(1) - p1(1)
        xt(2) = p2(2) - p1(2)

        xlen  = (xt(1)**2.d0 + xt(2)**2.d0)**0.5d0
        xt(1) = (xt(1) / xlen)
        xt(2) = (xt(2) / xlen)

        !Previous time step
        xt0(1) = p20(1) - p10(1)
        xt0(2) = p20(2) - p10(2)

        xlen0  = (xt0(1)**2.d0 + xt0(2)**2.d0)**0.5d0
        xt0(1) = (xt0(1) / xlen0)
        xt0(2) = (xt0(2) / xlen0)

        xn(1) = -1.d0*xt(2)
        xn(2) =  1.d0*xt(1)

        !Normal Gap Function

        gapn = ((ps(1) - p1(1)) * xn(1)) + ((ps(2) - p1(2)) * xn(2))

        !Check impenetration condition

        IsContact = .false.
        if ((gapn.ge.0.d0).or.(gapn.le.-1.d0*xlen)) then
            IsContact = .false.
        else
            IsContact = .true.
        end if

        !Calculate natural coordinate at contact point
        xi  = (((ps(1) - p1(1)) * xt(1)) + ((ps(2) - p1(2)) * xt(2))) / xlen

        xi0 = (((ps0(1) - p10(1)) * xt0(1)) + ((ps0(2) - p10(2)) * xt0(2))) / xlen0

        !Check contact is out of segment
        IsSegment = .false.

        if ((xi.lt.1.05d0).or.(xi.gt.-1.d0*0.05d0)) then
            IsSegment = .true.
        end if
       
        IsInterpentration = .false.
        if ((IsContact.eq..true.).and.(IsSegment.eq..true.)) then
            IsInterpentration = .true.
            xlambn = -1.d0 * MatProp(1) * gapn
            xlambt =  0.d0

            IsFric = .true.
            if (MatProp(3).eq.0.d0) then
                IsFric = .false.
            end if

            if (IsFric.eq..true.) then
                gapt = (xi - xi0) * xlen0
                xlambt = -1.d0 * MatProp(2) * gapt
                FRTOL = xlambn * MatProp(3)
                IsSlide = .false.
                if(abs(xlambt).gt.FRTOL) then
                    IsSlide = .true.
                    xlambt = -1.d0 * sign(1.d0,gapt)
                end if
            end if

            !Calculate the force

            Force(1) = xlambn * xn(1) + xlambt * xt(1) 
            Force(2) = xlambn * xn(2) + xlambt * xt(2)
            Force(3) = xlambn * ((xi-1)*xn(1) + gapn*xt(1)/xlen) + xlambt * ((xi-1)*xt(1) - gapn*xn(1)/xlen)
            Force(4) = xlambn * ((xi-1)*xn(2) + gapn*xt(2)/xlen) + xlambt * ((xi-1)*xt(2) - gapn*xn(2)/xlen)
            Force(5) = xlambn * (-1.d0*xi*xn(1) - gapn*xt(1)/xlen) + xlambt * (-1.d0*xi*xt(1) + gapn*xn(1)/xlen)
            Force(6) = xlambn * (-1.d0*xi*xn(2) - gapn*xt(2)/xlen) + xlambt * (-1.d0*xi*xt(2) + gapn*xn(2)/xlen)

        end if
 
end subroutine GetLocForce
    
    !**********************************************************************************************
    !   MAIN PROGRAM
    !**********************************************************************************************   
    
    program main
    implicit none
        double precision xs(2), x1(2), x2(2), MatProp(3), xs0(2), x10(2), x20(2), ForceRes(6)
        logical IsForceNonZero
        integer i
    
        ! Current nodal coordinates
        data (xs(i), i=1,2) / 1.d0, -1.d0 /
        data (x1(i), i=1,2) / 0.d0, 0.d0 /
        data (x2(i), i=1,2) / 2.d0, 0.d0 /
    
        ! Material properties
        data (MatProp(i), i=1,3) / 1.d0, 1.d0, 0.61d0 /
    
        ! Previous nodal coordinates
        data (xs0(i), i=1,2) / 1.d0, -1.d0 /
        data (x10(i), i=1,2) / 0.d0, 0.d0 /
        data (x20(i), i=1,2) / 2.d0, 0.1d0 /
    
        call GetLocForce(xs,x1,x2,MatProp,xs0,x10,x20,ForceRes,IsForceNonZero)
        
        open(1, file='output.dat')
        WRITE(1,*) xs
        WRITE(1,*) x1
        WRITE(1,*) x2
        WRITE(1,*) ForceRes
        WRITE(1,*) IsForceNonZero
    
    end program
    
    