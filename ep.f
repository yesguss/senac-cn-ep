!gfortran, gcc version 5.4.0 20160609

program ep

    interface
       
       function v_mult_escalar(s, m, n)
           INTEGER :: n
           REAL, dimension(n, 1) :: s
           REAL :: m
           REAL, dimension(n, 1) :: v_mult_escalar
       end function v_mult_escalar
	   
       function m_mult_escalar(A, m, n)
              INTEGER :: n
              REAL, dimension(n,n) :: A
              REAL :: m
              REAL, dimension(n,n) :: m_mult_escalar
       end function m_mult_escalar
	   
   			function mod_vector(A, n) result(r)
           INTEGER :: n
           REAL, dimension(n,1) :: A
           REAL :: r
       end function mod_vector
	   
       function add_vetor(Y, Z, n)
              INTEGER :: n
              REAL, dimension(n,1) :: Y
              REAL, dimension(n,1) :: Z
              REAL, dimension(n,1) :: add_vetor
       end function add_vetor
       
       function sub_vetor(Y, Z, n)
              INTEGER :: n
              REAL, dimension(n,1) :: Y
              REAL, dimension(n,1) :: Z
              REAL, dimension(n,1) :: sub_vetor
       end function sub_vetor
       
       function getA() result (A)
           REAL, dimension(8,8) :: A
       end function getA
       
       function getS() result (r)
           REAL, dimension(8,1) :: r
       end function getS
     
       function calc_aproxima(A, m, s, x_ant, n) result (R)
          INTEGER :: n
           REAL, dimension(n,n) :: A
           REAL :: m
           REAL, dimension(n,1) ::s
           REAL, dimension(n,1) :: x_ant
           REAL, dimension(n,1) :: R
       end function calc_aproxima
       
       function vetor_norm(a, n) result (R)
           INTEGER :: n
           REAL, dimension(n,n):: a
           REAL, dimension(n,n):: R
      end function vetor_norm

       subroutine imprimi_matriz(A, n)
           INTEGER :: n
           REAL, dimension(n,n) :: A
       end subroutine imprimi_matriz
       
       subroutine imprimi_vetor(A, n)
           INTEGER :: n
           REAL, dimension(n,1) :: A
       end subroutine imprimi_vetor
    end interface
    
    REAL, dimension(8,8) :: A
    REAL, dimension(8,1) :: s
    REAL, dimension(8,1) :: x_ant
    REAL, dimension(8,1) :: next_resp
    REAL :: m
    REAL :: threshold
    REAL :: error
    REAL :: module
    INTEGER :: n
    
    n = 8
    A = getA()
    s = getS()
    x_ant = getS()
    m = 0.15
    threshold = 0.001
    error = 1000
    
    DO WHILE (error .GT. threshold)
        next_resp = calc_aproxima(A, m, s, x_ant, n)
        error = mod_vector(sub_vetor(next_resp, x_ant, 8), 8)
        x_ant = next_resp
    END DO
    
    next_resp = vetor_norm(next_resp, n)
    
    call imprimi_vetor(next_resp, 8)
    
    module = mod_vector(next_resp, n)
    
    print *, module
end program ep

   function m_mult_escalar(A, m, n) result(X)

      INTEGER :: n
       REAL, dimension(n,n) :: A
       REAL :: m
        REAL, dimension(n,n) :: X
  
      do i=1,n
          do j=1,n
              X(i,j) = A(i,j) * m
          enddo
      enddo
    end function m_mult_escalar
    
    function v_mult_escalar(s, m, n) result(X)
        INTEGER :: n
         REAL, dimension(n, 1) :: s
        REAL :: m
         REAL, dimension(n, 1) :: X
        
        do i = 1,n
            X(i,1) = s(i,1) * m
        enddo
    end function v_mult_escalar
    
    function add_vetor(Y, Z, n) result(X)

    INTEGER :: n
      REAL, dimension(n,1) :: Y
      REAL, dimension(n,1) :: Z
      REAL, dimension(n,1) :: X
      
      do i=1,n
        X(i,1) = Y(i,1)+Z(i,1)
      enddo
    end function add_vetor
    
    function sub_vetor(Y, Z, n) result(X)

      INTEGER :: n
      REAL, dimension(n,1) :: Y
      REAL, dimension(n,1) :: Z
      REAL, dimension(n,1) :: X
      
      do i=1,n
        X(i,1) = Y(i,1)-Z(i,1)
      enddo
    end function sub_vetor
    
  
    function mod_vector(A, n) result(r)
       INTEGER :: n       
       REAL, dimension(n,1) :: A
       REAL :: module
       REAL :: r

       module = 0.0
       do i = 1, n
          module = module + A(i,1)*A(i,1)
       enddo
  
       r = sqrt(module)
    end function mod_vector
    
    function vetor_norm(A, n) result (r)
       interface
         function mod_vector(A, n)
		INTEGER :: n
              REAL, dimension(n,1) :: A
              REAL, dimension(n,1) :: r
         end function mod_vector
       end interface
       
		INTEGER :: n
       REAL, dimension(n, 1) :: A
       REAL, dimension(n, 1) :: R
       
       REAL :: module
       module = mod_vector(A, n)
       do i=1, n
           R(i, 1) = A(i,1) / module
       enddo
    end function
    
    
    function getA() result (A)
        REAL, dimension(8,8) :: A
        A(1,1) = 1
        A(1,2) = 12
        A(1,3) = 5
        A(1,4) = 0
        A(1,5) = 8
        A(1,6) = 0
        A(1,7) = 9
        A(1,8) = 7
        
        A(2,1) = 0
        A(2,2) = 0
        A(2,3) = 4
        A(2,4) = 0
        A(2,5) = 4
        A(2,6) = 0
        A(2,7) = 0
        A(2,8) = 4
        
        A(3,1) = 5
        A(3,2) = 5
        A(3,3) = 0
        A(3,4) = 4
        A(3,5) = 0
        A(3,6) = 0
        A(3,7) = 0
        A(3,8) = 5
        
        A(4,1) = 1
        A(4,2) = 2
        A(4,3) = 2
        A(4,4) = 3
        A(4,5) = 4
        A(4,6) = 5
        A(4,7) = 6
        A(4,8) = 7
        
        A(5,1) = 0
        A(5,2) = 0
        A(5,3) = 0
        A(5,4) = 0
        A(5,5) = 0
        A(5,6) = 0
        A(5,7) = 0
        A(5,8) = 0
        
        A(6,1) = 1
        A(6,2) = 1
        A(6,3) = 1
        A(6,4) = 1
        A(6,5) = 1
        A(6,6) = 1
        A(6,7) = 1
        A(6,8) = 1
        
        A(7,1) = 10
        A(7,2) = 20
        A(7,3) = 30
        A(7,4) = 40
        A(7,5) = 50
        A(7,6) = 60
        A(7,7) = 70
        A(7,8) = 80
        
        A(8,1) = 0
        A(8,2) = 0
        A(8,3) = 0
        A(8,4) = 0
        A(8,5) = 0
        A(8,6) = 0
        A(8,7) = 0
        A(8,8) = 0
    end function getA
    
    function getS() result (r)
        REAL, dimension(8,1) :: r
        
        r(1,1) = 0.125
        r(2,1) = 0.125
        r(3,1) = 0.125
        r(4,1) = 0.125
        r(5,1) = 0.125
        r(6,1) = 0.125
        r(7,1) = 0.125
        r(8,1) = 0.125
        
    end function getS
    
    function calc_aproxima(A, m, s, x_ant, n) result (R)
    
       interface
           function m_mult_escalar(A, m, n)
              INTEGER :: n
              REAL, dimension(n,n) :: A
              REAL :: m
              REAL, dimension(n,n) :: m_mult_escalar
           end function m_mult_escalar
       
         function v_mult_escalar(s, m, n)
           INTEGER :: n
           REAL, dimension(n, 1) :: s
           REAL :: m
           REAL, dimension(n, 1) :: v_mult_escalar
         end function v_mult_escalar
         
         function add_vetor(Y, Z, n)
              INTEGER :: n
              REAL, dimension(n,1) :: Y
              REAL, dimension(n,1) :: Z
              REAL, dimension(n,1) :: add_vetor
         end function add_vetor
       
       end interface
  
        INTEGER :: n
        REAL, dimension(n,n) :: A
        REAL :: m
        REAL, dimension(n,1) ::s
        REAL, dimension(n,1) :: x_ant
        REAL, dimension(n,1) :: R
        
       	   R = add_vetor(matmul(m_mult_escalar(A, 1-m, n), x_ant), v_mult_escalar(s, m, n), n)
    end function calc_aproxima
    
    subroutine imprimi_matriz(A, n)
      INTEGER :: n
      REAL, dimension(n,n) :: A
		
		do i=1,n
        do j=1,n
			print*, A(i,j)
        enddo
      enddo
    end subroutine imprimi_matriz
    
    subroutine imprimi_vetor(A, n)
		  INTEGER :: n
		  REAL, dimension(n,1) :: A
		  
		  do i=1,n
			print*, A(i,1)
		  enddo
    end subroutine imprimi_vetor