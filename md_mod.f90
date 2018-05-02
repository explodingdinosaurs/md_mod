module md_mod

  implicit none

  ! Global Variables
  integer, parameter :: ERROR_UNIT = 0
  
  ! Constants/Defaults
  double precision, parameter :: PI = 3.14159265

contains

!-------------------------------------------------------------------------------
!   Functions !!
!-------------------------------------------------------------------------------

  function cross(a, b)
    ! Calculates the cross product of two vectors
    real, dimension(3) :: cross
    real, dimension(3), intent(in) :: a, b
    
    cross(1) = a(2)*b(3) - a(3)*b(2)
    cross(2) = a(3)*b(1) - a(1)*b(3)
    cross(3) = a(1)*b(2) - a(2)*b(1)
  end function cross

!-------------------------------------------------------------------------------
  
  function mag(a)
    ! Calculates the length (magnitude) of a vector
    real :: mag
    real, dimension(3), intent(in) :: a
    
    mag = sqrt(a(1)**2 + a(2)**2 + a(3)**2)
    
  end function mag

!-------------------------------------------------------------------------------  
  
  function unit(a)
    ! Calculates the unit vector of a vector
    real, dimension(3) :: unit
    real, dimension(3), intent(in) :: a
    
    unit = a / mag(a)
  end function unit

!-------------------------------------------------------------------------------  
  
  function distance(a,b)
    ! Calculates the distance between two points
    real :: distance
    real, dimension(3), intent(in) :: a, b
    
    distance = sqrt((a(1)-b(1))**2 + (a(2)-b(2))**2 + (a(3)-b(3))**2)
  end function distance

!-------------------------------------------------------------------------------  

  function wrap_distance(p,q,a,b,c)
    ! Calculates distance between two points including wrapping
    ! p and q are coordinates
    ! a, b, c are unit vectors
    
    real :: wrap_distance
    real, dimension(3), intent(in) :: p, q
    real*8, intent(in) :: a, b, c
    real, dimension(27) :: dist
    
    !a, b, c are cell side lengths in x, y, z respectively

    
    dist(1) = sqrt((p(1)-q(1))**2 + (p(2)-q(2))**2 + (p(3)-q(3))**2)       ! 

    dist(2) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2))**2 + (p(3)-q(3))**2)     ! +x 
    dist(3) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2))**2 + (p(3)-q(3))**2)     ! -x
    dist(4) = sqrt((p(1)-q(1))**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3))**2)     ! +y
    dist(5) = sqrt((p(1)-q(1))**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3))**2)     ! -y
    dist(6) = sqrt((p(1)-q(1))**2 + (p(2)-q(2))**2 + (p(3)-q(3)+c)**2)     ! +z
    dist(7) = sqrt((p(1)-q(1))**2 + (p(2)-q(2))**2 + (p(3)-q(3)-c)**2)     ! -z

    dist(8) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3))**2)   ! +x, +y
    dist(9) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3))**2)   ! +x, -y  
    dist(10) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3))**2)   ! -x, +y
    dist(11) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3))**2)   ! -x, -y
    dist(12) = sqrt((p(1)-q(1))**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3)+c)**2)   ! +y, +z
    dist(13) = sqrt((p(1)-q(1))**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3)-c)**2)   ! +y, -z  
    dist(14) = sqrt((p(1)-q(1))**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3)+c)**2)   ! -y, +z
    dist(15) = sqrt((p(1)-q(1))**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3)-c)**2)   ! -y, -z
    dist(16) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2))**2 + (p(3)-q(3)+c)**2)   ! +x, +z
    dist(17) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2))**2 + (p(3)-q(3)-c)**2)   ! +x, -z  
    dist(18) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2))**2 + (p(3)-q(3)+c)**2)   ! -x, +z
    dist(19) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2))**2 + (p(3)-q(3)-c)**2)   ! -x, -z

    dist(20) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3)+c)**2)   ! +x, +y, +z
    dist(21) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3)-c)**2)   ! +x, +y, -z
    dist(22) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3)+c)**2)   ! +x, -y, +z
    dist(23) = sqrt((p(1)-q(1)+a)**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3)-c)**2)   ! +x, -y, -z
    dist(24) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3)+c)**2)   ! -x, +y, +z
    dist(25) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2)+b)**2 + (p(3)-q(3)-c)**2)   ! -x, +y, -z
    dist(26) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3)+c)**2)   ! -x, -y, +z
    dist(27) = sqrt((p(1)-q(1)-a)**2 + (p(2)-q(2)-b)**2 + (p(3)-q(3)-c)**2)   ! -x, -y, -z

    wrap_distance = minval(dist)


    
  end function wrap_distance

!-------------------------------------------------------------------------------

  function vector(p,q)
    real, dimension(3) :: vector
    real, dimension(3), intent(in) :: p, q

    vector(1) = p(1) - q(1)
    vector(2) = p(2) - q(2)
    vector(3) = p(3) - q(3)

  end function vector
!-------------------------------------------------------------------------------
  
  function wrap_vector(p,q,a_in,b_in,c_in)
    ! Calculates shortest distance vector between two points including wrapping
    ! p and q are coordinates
    ! a, b, c are unit vectors
    
    real, dimension(3) :: wrap_vector
    real, dimension(3), intent(in) :: p, q
    real*8, intent(in) :: a_in, b_in, c_in
    real :: a, b, c
    real, dimension(27,3) :: dist
    real :: smallest
    integer :: i, smallest_i
    
    !a, b, c are cell side lengths in x, y, z respectively

    a = real(a_in,4)
    b = real(b_in,4)
    c = real(c_in,4)
    
    dist(1,:) = [p(1)-q(1), p(2)-q(2), p(3)-q(3)]       ! 

    dist(2,:) = [p(1)-q(1)+a, p(2)-q(2), p(3)-q(3)]     ! +x 
    dist(3,:) = [p(1)-q(1)-a, p(2)-q(2), p(3)-q(3)]     ! -x
    dist(4,:) = [p(1)-q(1), p(2)-q(2)+b, p(3)-q(3)]     ! +y
    dist(5,:) = [p(1)-q(1), p(2)-q(2)-b, p(3)-q(3)]     ! -y
    dist(6,:) = [p(1)-q(1), p(2)-q(2), p(3)-q(3)+c]     ! +z
    dist(7,:) = [p(1)-q(1), p(2)-q(2), p(3)-q(3)-c]     ! -z

    dist(8,:) = [p(1)-q(1)+a, p(2)-q(2)+b, p(3)-q(3)]   ! +x, +y
    dist(9,:) = [p(1)-q(1)+a, p(2)-q(2)-b, p(3)-q(3)]   ! +x, -y  
    dist(10,:) = [p(1)-q(1)-a, p(2)-q(2)+b, p(3)-q(3)]   ! -x, +y
    dist(11,:) = [p(1)-q(1)-a, p(2)-q(2)-b, p(3)-q(3)]   ! -x, -y
    dist(12,:) = [p(1)-q(1), p(2)-q(2)+b, p(3)-q(3)+c]   ! +y, +z
    dist(13,:) = [p(1)-q(1), p(2)-q(2)+b, p(3)-q(3)-c]   ! +y, -z  
    dist(14,:) = [p(1)-q(1), p(2)-q(2)-b, p(3)-q(3)+c]   ! -y, +z
    dist(15,:) = [p(1)-q(1), p(2)-q(2)-b, p(3)-q(3)-c]   ! -y, -z
    dist(16,:) = [p(1)-q(1)+a, p(2)-q(2), p(3)-q(3)+c]   ! +x, +z
    dist(17,:) = [p(1)-q(1)+a, p(2)-q(2), p(3)-q(3)-c]   ! +x, -z  
    dist(18,:) = [p(1)-q(1)-a, p(2)-q(2), p(3)-q(3)+c]   ! -x, +z
    dist(19,:) = [p(1)-q(1)-a, p(2)-q(2), p(3)-q(3)-c]   ! -x, -z

    dist(20,:) = [p(1)-q(1)+a, p(2)-q(2)+b, p(3)-q(3)+c]   ! +x, +y, +z
    dist(21,:) = [p(1)-q(1)+a, p(2)-q(2)+b, p(3)-q(3)-c]   ! +x, +y, -z
    dist(22,:) = [p(1)-q(1)+a, p(2)-q(2)-b, p(3)-q(3)+c]   ! +x, -y, +z
    dist(23,:) = [p(1)-q(1)+a, p(2)-q(2)-b, p(3)-q(3)-c]   ! +x, -y, -z
    dist(24,:) = [p(1)-q(1)-a, p(2)-q(2)+b, p(3)-q(3)+c]   ! -x, +y, +z
    dist(25,:) = [p(1)-q(1)-a, p(2)-q(2)+b, p(3)-q(3)-c]   ! -x, +y, -z
    dist(26,:) = [p(1)-q(1)-a, p(2)-q(2)-b, p(3)-q(3)+c]   ! -x, -y, +z
    dist(27,:) = [p(1)-q(1)-a, p(2)-q(2)-b, p(3)-q(3)-c]   ! -x, -y, -z

    smallest=mag(dist(1,:))
    smallest_i=1
    
    do i=1, 27
       if (mag(dist(i,:)) .lt. smallest) then
          smallest = mag(dist(i,:))
          smallest_i = i
       end if
    end do
    
    wrap_vector = dist(smallest_i,:)

    
  end function wrap_vector

  !-------------------------------------------------------------------------------
  function dihedral(a,b,c,d)
    ! DO NOT USE - NOT FINISHED
    ! Calculates the dihedral angle between 4 points
    !  a--b
    !      \
    !       c--d
    
    real :: dihedral
    real, dimension(3), intent(in) :: a, b, c, d
    real, dimension(3) :: vec_ab, vec_bc, vec_cd
    real, dimension(3) :: norm_abc, norm_bcd

    vec_ab = vector(a,b)
    vec_bc = vector(b,c)
    vec_cd = vector(c,d)

    norm_abc = cross(vec_ab, vec_bc)
    norm_bcd = cross(vec_bc, vec_cd)
    
    dihedral = acos(-1*(dot_product(norm_abc, norm_bcd) / &
         (mag(norm_abc)*mag(norm_bcd))))
    

    
  end function dihedral
  
!-------------------------------------------------------------------------------


  
!-------------------------------------------------------------------------------
!   Subroutines 
!-------------------------------------------------------------------------------



!-------------------------------------------------------------------------------

  



!-------------------------------------------------------------------------------
!  Open input file, stop if the input file is not found
!-------------------------------------------------------------------------------
    subroutine open_input_file_stop_if_not_found(input_file_unit, input_filename)
      integer, intent(in) :: input_file_unit
      character(len=80), intent(in) :: input_filename
            
      logical :: file_exists
      
      inquire(file=input_filename, exist=file_exists)
      if (file_exists) then
        open(input_file_unit,file=input_filename,status='old',form='formatted')
      else
        write(ERROR_UNIT,*) "Unable to open file: ", input_filename
        stop
      end if

    end subroutine open_input_file_stop_if_not_found


!-------------------------------------------------------------------------------
!  Open output file
!-------------------------------------------------------------------------------
   
    subroutine open_output_file(output_file_unit, output_filename)
      integer, intent(in) :: output_file_unit
      character*80, intent(in) :: output_filename

      open(output_file_unit, file=output_filename)
      
    end subroutine open_output_file
    

!-------------------------------------------------------------------------------
!  Get filename suffix - everything before the last full stop
!  # CHECK THIS ONE - I DONT THINK ITS USED ANYWHERE AND MAY NOT WORK
!-------------------------------------------------------------------------------

    subroutine get_filename_suffix(filename,suffix)
      character*80, intent(in) :: filename
      character*80, intent(out) :: suffix

      integer :: pos

      ! Determine position of full stop
      pos = scan(trim(filename),".", BACK= .true.)

      ! Define prefix of filename as everything after full stop
      if ( pos > 0 ) suffix = filename(1:pos-1)

    end subroutine get_filename_suffix


!-------------------------------------------------------------------------------
!  Get filename extension - everything after the last full stop
!-------------------------------------------------------------------------------

    subroutine get_filename_extension(filename, ext)
      character*80, intent(in) :: filename
      character*80, intent(out) :: ext

      integer :: pos

      ! Determine position of full stop
      pos = scan(trim(filename),".", BACK= .true.)

      ! Define prefix of filename as everything after full stop
      if ( pos > 0 ) ext = filename(pos+1:len(filename))

    end subroutine get_filename_extension


!-------------------------------------------------------------------------------
!  Open trajectory file
!  Currently only supports dcd and xyz files. Might add xtc if I'm having
!  a good day.
!-------------------------------------------------------------------------------

    subroutine open_traj_file(traj_file_unit, traj_filename, traj_type)

      integer, intent(in) :: traj_file_unit
      character*80, intent(in) :: traj_filename
      character*80, intent(out) :: traj_type

      character*80 :: ext
      logical :: file_exists

      
      ! See if trajectory file exists. If doesn't, stop
      inquire(file=traj_filename, exist=file_exists)
      if (file_exists) then
         ! Get trajectory type
         call get_filename_extension(traj_filename, traj_type)
         traj_type = trim(adjustl(traj_type))

         if (traj_type == "dcd") then 
            open(traj_file_unit,file=traj_filename,status='old',form='unformatted')
         elseif (traj_type == "xyz") then
            open(traj_file_unit,file=traj_filename,status='old',form='formatted')
         end if
      else
         write(ERROR_UNIT,*) "Unable to open file: ", traj_filename
         stop
      end if
      
    end subroutine open_traj_file


!-------------------------------------------------------------------------------
!   Read traj header
!   Only supports dcd and xyz files. Use catdcd to convert others to dcd    
!-------------------------------------------------------------------------------

    subroutine read_traj_header(traj_file_unit, traj_type, n_frames, n_atoms)
      integer, intent(in) :: traj_file_unit
      character*80, intent(in) :: traj_type
      integer, intent(out) :: n_frames, n_atoms
      
      character*4 :: dum

      if (traj_type == "dcd") then 
         read(traj_file_unit) dum, n_frames
         read(traj_file_unit)
         read(traj_file_unit) n_atoms
      elseif (traj_type == "xyz") then
         ! Get n_atoms out
         read(traj_file_unit,*) n_atoms
         rewind(traj_file_unit)
      elseif ((traj_type == "xtc") .or. (traj_type == "trr")) then
         write(ERROR_UNIT,*) "Gromacs trajectories currently unsupported."   
         write(ERROR_UNIT,*) "Just use a good MD engine FFS."   
         stop
      else 
         write(ERROR_UNIT,*) "You done gone fucked up."
      end if   
   
    end subroutine read_traj_header


!-------------------------------------------------------------------------------
!   Read frame of trajectory
!   Only supports dcd and xyz files. Use catdcd to convert others to dcd    
!-------------------------------------------------------------------------------

    subroutine read_traj_frame(traj_file_unit, traj_type, n_atoms, coords, cell_basis_vectors)
      integer, intent(in) :: traj_file_unit
      integer, intent(inout) :: n_atoms
      character*80, intent(in) :: traj_type
      real, dimension(:,:), allocatable, intent(out) :: coords
      real*8, dimension(2,3), intent(out) :: cell_basis_vectors

      ! cell_basis_vectors is an array of form:
      ! --                   --
      ! | a_min, b_min, c_min |
      ! | a_max, b_max, c_max |
      ! --                   --

      real*8 :: a_min, a_max, b_min, b_max, c_min, c_max
      integer :: i, timestep
      real :: dum
      character :: name

      ! define coords array
      allocate(coords(n_atoms,3))

      if (traj_type == "dcd") then 
         ! ### NEXT READ LINE NEEDS TESTING
         read(traj_file_unit) a_min, a_max, b_min, b_max, c_min, c_max
         read(traj_file_unit) (coords(i,1), i=1, n_atoms)
         read(traj_file_unit) (coords(i,2), i=1, n_atoms) 
         read(traj_file_unit) (coords(i,3), i=1, n_atoms)

         ! Fill cell_basis_vectors array
         cell_basis_vectors(1,:) = [a_min, b_min, c_min]
         cell_basis_vectors(2,:) = [a_max, b_max, c_max]

      elseif (traj_type == "xyz") then 

         read(traj_file_unit,*) n_atoms!, dum, timestep
         read(traj_file_unit,*) a_max, b_max, c_max
         do i=1, n_atoms
            read(traj_file_unit,*) name, coords(i,1), coords(i,2), coords(i,3)
         end do
         
         cell_basis_vectors(1,:) = [0, 0, 0]
         cell_basis_vectors(2,:) = [a_max, b_max, c_max]
         
      elseif ((traj_type == "xtc") .or. (traj_type == "trr")) then
         write(ERROR_UNIT,*) "Gromacs trajectories currently unsupported."   
         write(ERROR_UNIT,*) "Just use a good MD engine FFS."   
         stop
      else 
         write(ERROR_UNIT,*) "You done gone fucked up."
      end if   

      
    end subroutine read_traj_frame

    
!-------------------------------------------------------------------------------
!   Close file
!-------------------------------------------------------------------------------
    subroutine close_file(output_file_unit)
      integer, intent(in) :: output_file_unit
      close(output_file_unit)
    end subroutine close_file


!-------------------------------------------------------------------------------
!   Read the config input file. Input file should have the following format:
!   - pdb file name
!   - trajectory file name
!   - cutoff distance (A)    
!   - resname of interest
!   - number of residues for each resname
!   - number of atoms in each residue, for each resname    
!   - Number of atoms in each of the below categories: eg 6 2 1 6 2 1
!   - Atom names making base of vector on 1st molecule. Eg:
!     C2 C3 C4 C5 C6 C7     
!   - Atom names making tip of lateral vector of 1st mol
!   - Atom names making tip of orthogonal vector of 1st mol.
!   - Atom names making base of vector on 2nd mol 
!   - Atom names making tip of lateral vector of 2nd mol
!   - Atom names making tip of orthogonal vector of 2nd mol.
!-------------------------------------------------------------------------------
    subroutine read_aof_conf_file(conf_file_unit, pdb_filename, traj_filename, cutoff_dist, &
         resnames_of_interest, n_molecules, atoms_per_molecule,&
         base_vec_names1, lat_vec_names1, orth_vec_names1,&
         base_vec_names2, lat_vec_names2, orth_vec_names2)

      integer, intent(in) :: conf_file_unit
      character*80, intent(out) :: pdb_filename, traj_filename
      real, intent(out) :: cutoff_dist
      character*4, dimension(:), allocatable, intent(out) :: resnames_of_interest
      character*4, dimension(:), allocatable, intent(out) :: base_vec_names1, lat_vec_names1, orth_vec_names1
      character*4, dimension(:), allocatable, intent(out) :: base_vec_names2, lat_vec_names2, orth_vec_names2
      integer, dimension(:), allocatable, intent(out) :: n_molecules, atoms_per_molecule

      
      integer, dimension(6) :: n_atoms_defining_vectors
      integer :: i

      allocate(resnames_of_interest(2))
      allocate(n_molecules(2))
      allocate(atoms_per_molecule(2))

      ! initilise n_molecules(2) 
      n_molecules(2) = 0
      
      ! Read config file
      read(conf_file_unit,*) pdb_filename
      read(conf_file_unit,*) traj_filename
      read(conf_file_unit,*) cutoff_dist
      read(conf_file_unit,*) n_molecules(1), n_molecules(2)

      if (n_molecules(2) == 0) then
         read(conf_file_unit,*) resnames_of_interest(1)
         resnames_of_interest(2) = resnames_of_interest(1)
         read(conf_file_unit,*) atoms_per_molecule(1)
         atoms_per_molecule(2) = atoms_per_molecule(1)
         read(conf_file_unit,*) (n_atoms_defining_vectors(i), i=1, 3) 
         n_atoms_defining_vectors(4) = n_atoms_defining_vectors(1)
         n_atoms_defining_vectors(5) = n_atoms_defining_vectors(2)
         n_atoms_defining_vectors(6) = n_atoms_defining_vectors(3)
         
      else
         read(conf_file_unit,*) resnames_of_interest(1), resnames_of_interest(2)
         read(conf_file_unit,*) atoms_per_molecule(1), atoms_per_molecule(2)
         read(conf_file_unit,*) (n_atoms_defining_vectors(i), i=1, 6) 

      end if

      ! NAmes of atoms defining various COGs
      allocate(base_vec_names1(n_atoms_defining_vectors(1)))
      allocate(lat_vec_names1(n_atoms_defining_vectors(2)))
      allocate(orth_vec_names1(n_atoms_defining_vectors(3)))
      allocate(base_vec_names2(n_atoms_defining_vectors(4)))
      allocate(lat_vec_names2(n_atoms_defining_vectors(5)))
      allocate(orth_vec_names2(n_atoms_defining_vectors(6)))

      
      do i=1, size(base_vec_names1)
         if(i .lt. size(base_vec_names1)) then 
            read(conf_file_unit,'(A4)',advance='no') base_vec_names1(i)
         else
            read(conf_file_unit,'(A4)') base_vec_names1(i)
            base_vec_names1(i) = adjustl(base_vec_names1(i))
         end if
      end do

      do i=1, size(lat_vec_names1)
         if(i .lt. size(lat_vec_names1)) then 
            read(conf_file_unit,'(A4)',advance='no') lat_vec_names1(i)
         else
            read(conf_file_unit,'(A4)') lat_vec_names1(i)
         end if
      end do

      do i=1, size(orth_vec_names1)
         if(i .lt. size(orth_vec_names1)) then 
            read(conf_file_unit,'(A4)',advance='no') orth_vec_names1(i)
         else
            read(conf_file_unit,'(A4)') orth_vec_names1(i)
         end if
      end do


      if (n_molecules(2)==0) then
         base_vec_names2 = base_vec_names1
         lat_vec_names2 = lat_vec_names1
         orth_vec_names2 = orth_vec_names1
      else
         do i=1, size(base_vec_names2)
            if(i .lt. size(base_vec_names2)) then 
               read(conf_file_unit,'(A4)',advance='no') base_vec_names2(i)
            else
               read(conf_file_unit,'(A4)') base_vec_names2(i)
            end if
         end do
         
         do i=1, size(lat_vec_names2)
            if(i .lt. size(lat_vec_names2)) then 
               read(conf_file_unit,'(A4)',advance='no') lat_vec_names2(i)
            else
               read(conf_file_unit,'(A4)') lat_vec_names2(i)
            end if
         end do
         
         do i=1, size(orth_vec_names2)
            if(i .lt. size(orth_vec_names2)) then 
               read(conf_file_unit,'(A4)',advance='no') orth_vec_names2(i)
            else
               read(conf_file_unit,'(A4)') orth_vec_names2(i)
            end if
         end do

      end if

      
    end subroutine read_aof_conf_file



    
!-------------------------------------------------------------------------------
!   Read pqr file 
!-------------------------------------------------------------------------------
    subroutine read_pqr_file(pqr_file_unit, atomtype, coords, atom, restype, resid, chain, n_atoms, o, b)
      integer, intent(in) :: pqr_file_unit
      integer, dimension(:), allocatable, intent(out) :: resid 
      real, dimension(:,:), allocatable, intent(out) :: coords
      real, dimension(:), allocatable, intent(out) :: o, b
      character(4), dimension(:), allocatable, intent(out) :: atomtype, atom, restype, chain
      integer, intent(out) :: n_atoms
      
      character(4) :: temp
      integer :: i, itemp, n_lines, j

      n_atoms = 0
      n_lines = 0
      
      ! Calculate number of atoms
      do while (1 .gt. 0)
         read(pqr_file_unit,*,END=5000) temp

         if (temp == "ATOM") then
            n_atoms = n_atoms + 1
         end if
         n_lines = n_lines + 1
      end do
      
5000  continue

      rewind(pqr_file_unit)
      
      ! Allocate atomic properties arrays
      allocate(coords(n_atoms,3))
      allocate(atomtype(n_atoms))
      allocate(atom(n_atoms))
      allocate(restype(n_atoms))
      allocate(resid(n_atoms))
      allocate(chain(n_atoms))
      allocate(o(n_atoms))
      allocate(b(n_atoms))

      ! If line starts with "ATOM" read line, add properties to arrays. 
      i=1

      do j=1, n_lines
         read(pqr_file_unit,*) temp
         if (temp == "ATOM" .or. temp == "HETA") then
            backspace(pqr_file_unit)
            read(pqr_file_unit,210) temp, itemp, atomtype(i), restype(i), &
                 chain(i), resid(i), coords(i,1), coords(i,2), coords(i,3), &
                 o(i), b(i)
            i=i+1
         end if
      end do

210   format(A4,2x,I5,2x,A4,A4,A1,I4,4x,3F8.3,2F6.2)        
    end subroutine read_pqr_file
      
!-------------------------------------------------------------------------------
!   Read pdb file - same as read_pqr_file just with pqr replaced with pdb
!-------------------------------------------------------------------------------
    subroutine read_pdb_file(pdb_file_unit, atomtype, coords, atom, restype, resid, chain, n_atoms, o, b)
      integer, intent(in) :: pdb_file_unit
      integer, dimension(:), allocatable, intent(out) :: resid 
      real, dimension(:,:), allocatable, intent(out) :: coords
      real, dimension(:), allocatable, intent(out) :: o, b
      character(4), dimension(:), allocatable, intent(out) :: atomtype, atom, restype, chain
      integer, intent(out) :: n_atoms
      
      character(4) :: temp
      integer :: i, itemp, n_lines, j
      
      n_atoms = 0
      n_lines = 0
      
      ! Calculate number of atoms
      do while (1 .gt. 0)
         read(pdb_file_unit,*,END=5000) temp

         if (temp == "ATOM" .or. temp == "HETA") then
            n_atoms = n_atoms + 1
         end if
         n_lines = n_lines + 1
      end do
      
5000  continue

      rewind(pdb_file_unit)
      
      ! Allocate atomic properties arrays
      allocate(coords(n_atoms,3))
      allocate(atomtype(n_atoms))
      allocate(atom(n_atoms))
      allocate(restype(n_atoms))
      allocate(resid(n_atoms))
      allocate(chain(n_atoms))
      allocate(o(n_atoms))
      allocate(b(n_atoms))


      ! If line starts with "ATOM" read line, add properties to arrays. 
      i=1

      do j=1, n_lines
         read(pdb_file_unit,*) temp
         if (temp == "ATOM" .or. temp == "HETA") then
            backspace(pdb_file_unit)
            read(pdb_file_unit,210) temp, itemp, atomtype(i), restype(i), &
                 chain(i), resid(i), coords(i,1), coords(i,2), coords(i,3), &
                 o(i), b(i)
            i=i+1
         end if
      end do

210   format(A4,2x,I5,1x,A4,1x,A4,A1,I4,4x,3F8.3,2F6.2)        
    end subroutine read_pdb_file

    
!-------------------------------------------------------------------------------
!   Write to pqr file 
!-------------------------------------------------------------------------------
    subroutine write_to_output_pqr(pqr_file_unit, start_index, atomtype, coords, atom, restype, resid, chain, n_atoms, o, b)

      integer, intent(in) :: pqr_file_unit, start_index
      integer, dimension(:), intent(in) :: resid 
      real, dimension(:,:), intent(in) :: coords    
      character(4), dimension(:), intent(in) :: atomtype, atom, restype, chain
      integer, intent(in) :: n_atoms
      real, dimension(:), allocatable, intent(in) :: o, b
      
      integer :: i
      
      do i=1, n_atoms
      
         write(pqr_file_unit,211) "ATOM", i+start_index-1, atomtype(i),&
              trim(adjustl(restype(i))), chain(i), resid(i),&
              coords(i,1), coords(i,2), coords(i,3), o(i), b(i)
                  
      end do

      
211   format(A4,2x,I5,2x,A3,A4,1x,A1,I4,4x,3F8.3,2F6.2)     
    end subroutine write_to_output_pqr


!-------------------------------------------------------------------------------
!   Write to pdb file 
!-------------------------------------------------------------------------------
    subroutine write_to_output_pdb(pdb_file_unit, start_index, atomtype, coords, atom, restype, resid, chain, n_atoms, o, b)

      integer, intent(in) :: pdb_file_unit, start_index
      integer, dimension(:), intent(in) :: resid 
      real, dimension(:,:), intent(in) :: coords    
      character(4), dimension(:), intent(in) :: atomtype, atom, restype, chain
      integer, intent(in) :: n_atoms
      real, dimension(:), allocatable, intent(in) :: o, b
      
      integer :: i
      
      do i=1, n_atoms
      
         write(pdb_file_unit,211) "ATOM", i+start_index-1, atomtype(i),&
              trim(adjustl(restype(i))), chain(i), resid(i),&
              coords(i,1), coords(i,2), coords(i,3), o(i), b(i)
                  
      end do

      
211   format(A4,2x,I5,2x,A3,A4,1x,A1,I4,4x,3F8.3,2F6.2)     
    end subroutine write_to_output_pdb


!-------------------------------------------------------------------------------
!   Translate atomic coordinates
!-------------------------------------------------------------------------------
  subroutine translate_coords(coords_in, trans_vector, coords_out)

    real, dimension(:,:), intent(in) :: coords_in
    real, dimension(:,:), allocatable, intent(out) :: coords_out
    real, dimension(3), intent(in) :: trans_vector
    integer :: i

    allocate(coords_out(size(coords_in,1), size(coords_in,2)))

!    print*, "Trans_vector:", trans_vector
!    print*, coords_in(1,1), coords_in(1,2), coords_in(1,3)
    do i=1, size(coords_in,1)
       coords_out(i,1) = coords_in(i,1) + trans_vector(1)
       coords_out(i,2) = coords_in(i,2) + trans_vector(2)
       coords_out(i,3) = coords_in(i,3) + trans_vector(3)
    end do

!    print*, coords_out(1,1), coords_out(1,2), coords_out(1,3)
    
  end subroutine translate_coords


!-------------------------------------------------------------------------------
!   ROTATE ( Generate a rotation matrix )
! from ftp://naif.jpl.nasa.gov/pub/naif/toolkit/FORTRAN/MacIntel_OSX_gfortran_64bit/packages/
! and in
! /Users/pickle/bin/cspice/toolkit/src/spicelib
!-------------------------------------------------------------------------------
  SUBROUTINE ROTATE ( ANGLE, IAXIS, MOUT )
 
!$ Abstract
!
!      Calculate the 3x3 rotation matrix generated by a rotation
!      of a specified angle about a specified axis. This rotation
!      is thought of as rotating the coordinate system.
 
      DOUBLE PRECISION   ANGLE
      INTEGER            IAXIS
      DOUBLE PRECISION   MOUT  ( 3,3 )
 
!$ Brief_I/O
!
!      VARIABLE  I/O              DESCRIPTION
!      --------  ---  --------------------------------------------------
!       ANGLE     I     Angle of rotation (radians).
!       IAXIS     I     Axis of rotation (X=1, Y=2, Z=3).
!       MOUT      O     Resulting rotation matrix [ANGLE]
!                                                       IAXIS
!$ Detailed_Input
!
!      ANGLE   The angle given in radians, through which the rotation
!              is performed.
!
!      IAXIS   The index of the axis of rotation.  The X, Y, and Z
!              axes have indices 1, 2 and 3 respectively.
!
!$ Detailed_Output
!
!      MOUT    Rotation matrix which describes the rotation of the
!               COORDINATE system through ANGLE radians about the
!               axis whose index is IAXIS.
!
!$ Parameters
!
!      None.
!
!$ Particulars
!
!      A rotation about the first, i.e. x-axis, is described by
!
!      |  1        0          0      |
!      |  0   cos(theta) sin(theta)  |
!      |  0  -sin(theta) cos(theta)  |
!
!      A rotation about the second, i.e. y-axis, is described by
!
!      |  cos(theta)  0  -sin(theta)  |
!      |      0       1        0      |
!      |  sin(theta)  0   cos(theta)  |
!
!      A rotation about the third, i.e. z-axis, is described by
!
!      |  cos(theta) sin(theta)   0   |
!      | -sin(theta) cos(theta)   0   |
!      |       0          0       1   |
!
!      ROTATE decides which form is appropriate according to the value
!      of IAXIS.
!
!$ Examples
!
!      If ROTATE is called from a FORTRAN program as follows:
!
!            CALL ROTATE (PI/4, 3, MOUT)
!
!      then MOUT will be given by
!
!                   | SQRT(2)/2   SQRT(2)/2   0  |
!            MOUT = |-SQRT(2)/2   SQRT(2)/2   0  |
!                   |     0           0       1  |
!
      DOUBLE PRECISION      S
      DOUBLE PRECISION      C
 
      INTEGER               TEMP
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               INDEXS ( 5 )
      SAVE                  INDEXS
 
      DATA                  INDEXS  / 3, 1, 2, 3, 1 /
!
!  Get the sine and cosine of ANGLE
!
      S = DSIN(ANGLE)
      C = DCOS(ANGLE)
!
!  Get indices for axes. The first index is for the axis of rotation.
!  The next two axes follow in right hand order (XYZ).  First get the
!  non-negative value of IAXIS mod 3 .
!
      TEMP = MOD ( MOD(IAXIS,3) + 3, 3 )
 
      I1 = INDEXS( TEMP + 1 )
      I2 = INDEXS( TEMP + 2 )
      I3 = INDEXS( TEMP + 3 )
 
!
!  Construct the rotation matrix
!
      MOUT(I1,I1) = 1.D0
      MOUT(I2,I1) = 0.D0
      MOUT(I3,I1) = 0.D0
      MOUT(I1,I2) = 0.D0
      MOUT(I2,I2) = C
      MOUT(I3,I2) = -S
      MOUT(I1,I3) = 0.D0
      MOUT(I2,I3) = S
      MOUT(I3,I3) = C

      RETURN
    END SUBROUTINE ROTATE  




!-------------------------------------------------------------------------------
!   get centre of geometry of set of coordinates
!-------------------------------------------------------------------------------
    subroutine get_coords_from_index(coords, index, coords_out)
      real, dimension(:,:), intent(in) :: coords
      integer, intent(in) :: index
      real, dimension(3), intent(out) :: coords_out

      coords_out = coords(index,:)

    end subroutine get_coords_from_index
    

!-------------------------------------------------------------------------------
!   get centre of geometry of set of coordinates
!-------------------------------------------------------------------------------
    subroutine get_cog(coords, cog)
      real, dimension(:,:), intent(in) :: coords
      real, dimension(3), intent(out) :: cog

      cog(1) = sum(coords(:,1))/size(coords,1)
      cog(2) = sum(coords(:,2))/size(coords,1)
      cog(3) = sum(coords(:,3))/size(coords,1)
            
    end subroutine get_cog


!-------------------------------------------------------------------------------
!   get centre of geometry of NTER coordinates
!-------------------------------------------------------------------------------
    subroutine get_NTER_cog(coords, resid, cog)
      real, dimension(:,:), intent(in) :: coords
      integer, dimension(:), intent(in) :: resid
      real, dimension(3), intent(out) :: cog

      integer :: i, nter_resid, n_nter_atoms

      ! Assume that NTER is first residue in the pdb file. 

      n_nter_atoms = 0
      nter_resid = minval(resid)

      ! Get number of atoms in NTER residue
      do i=1, size(resid,1)
         if (resid(i) == nter_resid) then
            n_nter_atoms = n_nter_atoms + 1
         end if
      end do

      cog(1) = sum(coords(1:n_nter_atoms,1))/n_nter_atoms
      cog(2) = sum(coords(1:n_nter_atoms,2))/n_nter_atoms
      cog(3) = sum(coords(1:n_nter_atoms,3))/n_nter_atoms

    end subroutine get_NTER_cog


!-------------------------------------------------------------------------------
!   get centre of geometry of CTER coordinates
!-------------------------------------------------------------------------------
    subroutine get_CTER_cog(coords, resid, cog)
      real, dimension(:,:), intent(in) :: coords
      integer, dimension(:), intent(in) :: resid
      real, dimension(3), intent(out) :: cog

      integer :: i, cter_resid, n_cter_atoms

      ! Assume that CTER is last residue in the pqr file. 

      n_cter_atoms = 0
      cter_resid = maxval(resid)

      ! Get number of atoms in NTER residue
      do i=1, size(resid,1)
         if (resid(i) == cter_resid) then
            n_cter_atoms = n_cter_atoms + 1
         end if
      end do

      cog(1) = sum(coords(size(resid,1)-n_cter_atoms+1:size(resid,1),1))/n_cter_atoms
      cog(2) = sum(coords(size(resid,1)-n_cter_atoms+1:size(resid,1),2))/n_cter_atoms
      cog(3) = sum(coords(size(resid,1)-n_cter_atoms+1:size(resid,1),3))/n_cter_atoms

    end subroutine get_CTER_cog



!-------------------------------------------------------------------------------
!   Get NTER nitrogen index
!-------------------------------------------------------------------------------
    subroutine get_NTER_N_index(resid, atomtype, index_out)
      integer, dimension(:), intent(in) :: resid
      character(4), dimension(:), intent(in) :: atomtype
      integer, intent(out) :: index_out

      integer :: i, nter_resid

      nter_resid = minval(resid)
      
      ! Search through all atoms looking for last resid and atomtype N
      do i=1, size(resid,1)
         if (resid(i) == nter_resid .and. trim(adjustl(atomtype(i))) == "N") then
            index_out = i
            exit
         end if
      end do

    end subroutine get_NTER_N_index

!-------------------------------------------------------------------------------
!   get CTER carbon index
!-------------------------------------------------------------------------------

   subroutine get_CTER_C_index(resid, atomtype, index_out)
      integer, dimension(:), intent(in) :: resid
      character(4), dimension(:), intent(in) :: atomtype
      integer, intent(out) :: index_out

      integer :: i, cter_resid
      
      cter_resid = maxval(resid)

      ! Search through all atoms looking for last resid and atomtype N
      do i=1, size(resid,1)
         if (resid(i) == cter_resid .and. trim(adjustl(atomtype(i))) == "C") then
            index_out = i
            exit
         end if
      end do

    end subroutine get_CTER_C_index


!-------------------------------------------------------------------------------
!   get NTER nitrogen coordinates
!-------------------------------------------------------------------------------

    subroutine get_NTER_N_coords(coords, resid, atomtype, coords_out)
      real, dimension(:,:), intent(in) :: coords
      integer, dimension(:), intent(in) :: resid
      character(4), dimension(:), intent(in) :: atomtype
      real, dimension(3), intent(out) :: coords_out

      integer :: index
      
      call get_NTER_N_index(resid, atomtype, index)
      call get_coords_from_index(coords, index, coords_out)
      
    end subroutine get_NTER_N_coords

!-------------------------------------------------------------------------------
!   get CTER carbon coordinates
!-------------------------------------------------------------------------------

   subroutine get_CTER_C_coords(coords, resid, atomtype, coords_out)
      real, dimension(:,:), intent(in) :: coords
      integer, dimension(:), intent(in) :: resid
      character(4), dimension(:), intent(in) :: atomtype
      real, dimension(3), intent(out) :: coords_out
      
      integer :: index
      
      call get_CTER_C_index(resid, atomtype, index)
      call get_coords_from_index(coords, index, coords_out)
     
    end subroutine get_CTER_C_coords
        
    
!-------------------------------------------------------------------------------
!   get_TER_coords - call get_NTER_N_coords if n is supplied
!   call get_CTER_C_coords  if c is supplied. 
!-------------------------------------------------------------------------------   

    subroutine get_TER_coords(coords, resid, atomtype, n_or_c, coords_out)
      real, dimension(:,:), intent(in) :: coords
      integer, dimension(:), intent(in) :: resid
      character(4), dimension(:), intent(in) :: atomtype
      character(1), intent(in) :: n_or_c
      real, dimension(3), intent(out) :: coords_out
      
      if (n_or_c == 'n') then
         call get_NTER_N_coords(coords, resid, atomtype, coords_out)
      elseif (n_or_c == 'c') then
         call get_CTER_C_coords(coords, resid, atomtype, coords_out)
      else
         write(ERROR_UNIT,*) "ERROR: You didn't select n or c in get_TER_coords."
      end if
      
    end subroutine get_TER_coords
    


    
!-------------------------------------------------------------------------------
!   ROTATE_COORDS: rotate coordinates by first translating cog to origin, 
!   rotate using rotation matrix from ROTATE, then back to original position 
!
!   rot_point is the point about which the rotation is taking place  - cog, 
!             nter_cog, cter_cog etc. Is a 3-coordinate
!   angle is rotation angle in radians.
!   i_axis = 1, 2 or 3 corresponding to rotation about the x, y or z axis.
!-------------------------------------------------------------------------------
    subroutine rotate_coords(coords_in, rot_point, angle, i_axis, coords_out)
      real, dimension(:,:), intent(in) :: coords_in
      real, dimension(3), intent(in) :: rot_point
      real, dimension(:,:), allocatable, intent(out) :: coords_out
      double precision, intent(in) :: angle
      integer, intent(in) :: i_axis
      
      double precision :: rot_mat ( 3,3 )
      real, dimension(:,:), allocatable :: rot_coords

      
      ! Check to see if i_axis is a valid number (1, 2 or 3)
      if (i_axis == 1 .or. i_axis == 2 .or. i_axis == 3) then
         continue
      else
         write(ERROR_UNIT,*) "Rotation axis is not well defined. "
         write(ERROR_UNIT,*) "Needs to be an integer = 1, 2 or 3 to define x, y or z axis."
         stop
      end if

      allocate(rot_coords (size(coords_in,1),size(coords_in,2)) )
      
      ! Translate coords to origin via rot_point
      call translate_coords(coords_in, -1*rot_point, coords_out)
      
      ! determine rotation matrix for rotation
      call rotate(angle, i_axis, rot_mat)

      ! Apply rotation to coordinates
      rot_coords=matmul(coords_out, rot_mat)
      
      ! Translate coords back to original position 
      call translate_coords(rot_coords, rot_point, coords_out)
    
    end subroutine rotate_coords


!-------------------------------------------------------------------------------
!   ROTATE_COORDS: rotate coordinates by first translating cog to origin, 
!   rotate using rotation matrix from ROTATE, then back to original position 
!
!   rot_point is the point about which the rotation is taking place  - cog, 
!             nter_cog, cter_cog etc. Is a 3-coordinate
!   angle is rotation angle in radians.
!   i_axis = 1, 2 or 3 corresponding to rotation about the x, y or z axis.
!-------------------------------------------------------------------------------
    subroutine rotate_coords_with_rot_mat(coords_in, rot_point, rot_mat, coords_out)
      real, dimension(:,:), intent(in) :: coords_in
      real, dimension(3), intent(in) :: rot_point
      double precision, dimension(:,:), intent(in) :: rot_mat
      real, dimension(:,:), allocatable, intent(out) :: coords_out

      real, dimension(:,:), allocatable :: rot_coords

      
      allocate(rot_coords (size(coords_in,1),size(coords_in,2)) )
      
      ! Translate coords to origin via rot_point
      call translate_coords(coords_in, -1*rot_point, coords_out)
      
      ! Apply rotation to coordinates
      rot_coords=matmul(coords_out, rot_mat)
      
      ! Translate coords back to original position 
      call translate_coords(rot_coords, rot_point, coords_out)
    
    end subroutine rotate_coords_with_rot_mat


!-------------------------------------------------------------------------------
!   ARE_MATRICES_EQUAL: takes two nxn matrices and check if they are the same 
!   within a 0.0001 tolerance
!-------------------------------------------------------------------------------
    subroutine are_matrices_equal(mat1, mat2, equal)
      double precision, dimension(:,:), intent(in) :: mat1, mat2
      logical, intent(out) :: equal

      real, parameter :: tol=0.001
      integer :: i, j
            
      !check to see if dimensions are the same
      if ((size(mat1,1) .ne. size(mat2,1)) .or. (size(mat1,2) .ne. size(mat2,2))) then
         write(ERROR_UNIT,*) "are_matrices_equal was fed arrays of different sizes"
         stop
      end if

      !check to see if arrays are square
      if ((size(mat1,1) .ne. size(mat1,2)) .or. (size(mat2,1) .ne. size(mat2,2))) then
         write(ERROR_UNIT,*) "are_matrices_equal was fed non-square arrays"
         stop
      end if

      equal=.TRUE.
      
      !check to see if arrays are equal. If not, return false, else return true
      loop1: do i=1, size(mat1,1)
         do j=1, size(mat1,2)
            if ( (mat2(i,j) .lt. mat1(i,j)-tol) .or. (mat2(i,j) .gt. mat1(i,j)+tol) ) then
               equal=.FALSE.
               exit loop1
            end if
         end do
      end do loop1
      
    end subroutine are_matrices_equal

    
!-------------------------------------------------------------------------------
!   UNFINISHED!
!   random_rotate: uses rotate_coords to make three random rotations along
!   x, y, z axes
!
!   angle is rotation angle in radians.
!   i_axis = 1, 2 or 3 corresponding to rotation about the x, y or z axis.
!-------------------------------------------------------------------------------
!    subroutine random_rotate(coords_in, rot_point, coords_out)
!      real, dimension(:,:), intent(in) :: coords_in
!      real, dimension(3), intent(in) :: rot_point
!      real, dimension(:,:), intent(out) :: coords_out
!      
!      double precision :: random_angle
!      real, dimension(size(coords_in,1),size(coords_in,2)) :: coords_r, coords_rr
!      real :: r
!      
!      ! get a random number, multiply by 2*PI
!      call init_random_seed
!      call random_number(r)
!      random_angle = r*2*PI
!      
!      ! Do first rotation
!      call rotate_coords(coords_in, rot_point, random_angle, 1, coords_r)
!      
!      call init_random_seed
!      call random_number(r)
!      random_angle = r*2*PI
!      call rotate_coords(coords_r, rot_point, random_angle, 2, coords_rr)
!      
!      call init_random_seed
!      call random_number(r)
!      random_angle = r*2*PI
!      call rotate_coords(coords_rr, rot_point, random_angle, 3, coords_out)
!      
!   end subroutine random_rotate


!-------------------------------------------------------------------------------
!   return cog coordinates given an atomtype array and resid 
!-------------------------------------------------------------------------------
    subroutine get_resids_from_restype(restype_of_interest, restype, resid, n_molecules, resid_of_interest)
      character(len=4), intent(in) :: restype_of_interest
      character(len=4), dimension(:), intent(in) :: restype
      integer, dimension(:), intent(in) :: resid
      integer, intent(in) :: n_molecules
      integer, dimension(:), allocatable, intent(out) :: resid_of_interest
      
      integer :: i, j, k, resid_m1
      
      allocate(resid_of_interest(n_molecules))

      resid_of_interest = 0
      resid_m1 = 0
      k=1

      do j=1, size(restype)
         if (trim(adjustl(restype_of_interest)) == trim(adjustl(restype(j)))) then
            if (resid(j) .ne. resid_m1) then
               resid_m1 = resid(j)
               resid_of_interest(k) = resid(j)
               k=k+1
            else 
               cycle
            end if
         end if
      end do
      
    end subroutine get_resids_from_restype


    
!-------------------------------------------------------------------------------
!   return cog coordinates given an atomtype array and resid 
!-------------------------------------------------------------------------------

    subroutine get_cog_from_atomtype_and_resid(target_atomtype, target_resid, atomtype, resid, coords, cog)

      character(len=4), dimension(:), intent(in) :: target_atomtype, atomtype
      integer, intent(in) :: target_resid
      integer, dimension(:), intent(in) :: resid
      real, dimension(:,:), intent(in) :: coords
      real, dimension(3), intent(out) :: cog

      integer, dimension(:), allocatable :: ind
      real, dimension(:,:), allocatable :: target_coords
      integer :: i
      
      call create_atom_id_array(target_atomtype, target_resid, atomtype, resid, ind)
      call get_coords_from_atom_index(ind, coords, target_coords)
      call get_cog(target_coords, cog)

    end subroutine get_cog_from_atomtype_and_resid
    
  
!-------------------------------------------------------------------------------
!   create an array of atom ids given a list of names and resid
!-------------------------------------------------------------------------------

    subroutine create_atom_id_array(target_atomtype, target_resid, atomtype, resid, atom_ids)
      character*4, dimension(:), intent(in) :: target_atomtype, atomtype
      integer, intent(in) :: target_resid
      integer, dimension(:), intent(in) :: resid
      integer, dimension(:), allocatable, intent(out) :: atom_ids
      
      integer :: i, j
      
      allocate(atom_ids(size(target_atomtype)))

      do i=1, size(target_atomtype)
         do j=1, size(atomtype)
            if((trim(adjustl(target_atomtype(i))) == trim(adjustl(atomtype(j))))&
                 .and. (target_resid == resid(j))) then 
               atom_ids(i) = j
               exit
            end if
         end do
      end do

    end subroutine create_atom_id_array

!-------------------------------------------------------------------------------
!   get coordinates of atoms specified by an index array
!-------------------------------------------------------------------------------
    subroutine get_coords_from_atom_index(ind, coords, coords_out)
      integer, dimension(:), intent(in) :: ind
      real, dimension(:,:), intent(in) :: coords
      real, dimension(:,:), allocatable, intent(out) :: coords_out

      integer :: i
      
      ! Make output array first dimension of same length of input index
      ! array
      allocate(coords_out(size(ind), 3))

      ! Get coords out of coords array and assign to coords_out
      do i=1, size(coords_out, 1)
         coords_out(i,:) = coords(ind(i),:)
      end do


    end subroutine get_coords_from_atom_index


    


!-------------------------------------------------------------------------------
! split a string into 2 either side of a delimiter token
! taken from https://gist.github.com/ponderomotion/3833266
! CHECK - NOT SURE WHAT THIS SUBROUTINE DOES    
!-------------------------------------------------------------------------------
    subroutine get_resname_from_resid(resid, resid_list, resname_list, n_atoms, resname)
      integer, intent(in) :: resid
      integer, dimension(:), intent(in) :: resid_list
      character(4), dimension(:), intent(in) :: resname_list
      integer, intent(in) :: n_atoms
      character(4), intent(out) :: resname

      integer :: i

      resname = "ERRO"
      
      do i=1, n_atoms
         if (resid_list(i) == resid) then
            resname = resname_list(i)
            exit
         end if
      end do

      if (resname == "ERRO") then
        write(ERROR_UNIT,*) "resname of resid", resid, "not found. Exiting."
        stop
     end if

    end subroutine get_resname_from_resid
      

  end module md_mod
