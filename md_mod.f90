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
!   Read basename from commandline
!-------------------------------------------------------------------------------
    subroutine process_commandline(input_filename)
      character*80, intent(out) :: input_filename
      character(len=80) arg

      ! Read input_filename
      call get_command_argument(1, input_filename)
      if (len_trim(input_filename) == 0) call print_usage_then_stop

    end subroutine process_commandline


!-------------------------------------------------------------------------------
!   Print out the correct usage of commandline args and then stop
!-------------------------------------------------------------------------------
    subroutine print_usage_then_stop
      write(ERROR_UNIT,*) "Incorrect number of arguments. Correct Usage:"
      write(ERROR_UNIT,*) "bloberator <input_file>"
      write(ERROR_UNIT,*) "Example:"
      write(ERROR_UNIT,*) "bloberator input.in"
      stop
    end subroutine print_usage_then_stop


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
!  Open trajectory binary file
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
!   - pqr file 1, this should be the protein to which the linker is connected     
!     to the CTER
!   - pqr file 2, this is the protein to with the linker is connected to the 
!     NTER
!   - max length of the linker (also called the cutoff_distance
!   - grid spacing for translation (in angstrom)
!   - theta - angle by which protein is rotated in x axis (input file is degrees, 
!     subroutine outputs radians for internal goodness )   
!   - phi - angle by which protein is rotated in y axis (input file in degress,
!     subroutine outputs in radians)
!   - gamma - angle by which protein is rotated in y axis (input file in degress,
!     subroutine outputs in radians)    
!   - overlap detection cutoff (in angstrom)
!   - experimental constrains. integer. if 0, stop. If n != 0 then read next n
!     lines, index1, index2, center and tolerance    
!-------------------------------------------------------------------------------
    subroutine read_conf_file(conf_file_unit, pqr_filename1, pqr_filename2, &
         cutoff_dist, grid_spacing, theta, phi, gamma, overlap_dist,&
         n_coords_arrays_in_check_storage, exp_constraints)

      integer, intent(in) :: conf_file_unit
      character*80, intent(out) :: pqr_filename1, pqr_filename2
      real, intent(out) :: cutoff_dist, overlap_dist, grid_spacing      
      double precision, intent(out) :: theta, phi, gamma
      real, dimension(:,:), allocatable, intent(out) :: exp_constraints
      integer, intent(out) :: n_coords_arrays_in_check_storage
      
      integer :: i, n_exp_constraints
      double precision :: theta_deg, phi_deg, gamma_deg
      
      read(conf_file_unit,*) pqr_filename1
      read(conf_file_unit,*) pqr_filename2
      read(conf_file_unit,*) cutoff_dist
      read(conf_file_unit,*) grid_spacing
      read(conf_file_unit,*) theta_deg
      read(conf_file_unit,*) phi_deg
      read(conf_file_unit,*) gamma_deg
      read(conf_file_unit,*) overlap_dist
      read(conf_file_unit,*) n_coords_arrays_in_check_storage
      read(conf_file_unit,*) n_exp_constraints

      ! Is there are experimental constraints?
      ! If no.. create null array..???
      if (n_exp_constraints == 0 ) then
         ! Set null array?
         allocate(exp_constraints(0,0))
         ! If there are, allocate array
      else
         allocate(exp_constraints(n_exp_constraints,4))

         ! Read experimental constraints
         do i=1, n_exp_constraints
            read(conf_file_unit,*) exp_constraints(i,1), exp_constraints(i,2),&
                 exp_constraints(i,3), exp_constraints(i,4)
         end do
      end if
      
      ! Convert theta and phi from degrees to radians
      theta = (theta_deg * PI)/180
      phi = (phi_deg * PI)/180
      gamma = (gamma_deg * PI)/180
      
    end subroutine read_conf_file


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
!   Read pqr file - same as read_pqr_file just with pqr replaced with pdb
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

      ! Assume that NTER is first residue in the pqr file. 

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
!   get_small_protein: see if CTER or NTER protein is smallest.
!   Output n if nter is smallest, c if cter is smallest   
!-------------------------------------------------------------------------------   

    subroutine get_small_protein(coords1, coords2, small_n_or_c, big_n_or_c)
      real, dimension(:,:), intent(in) :: coords1, coords2
      character(1), intent(out) :: small_n_or_c, big_n_or_c

      if (size(coords1,1) .le. size(coords2,1)) then
         small_n_or_c = "c"
         big_n_or_c = "n"
      else
         small_n_or_c = "n"
         big_n_or_c = "c"
      end if

    end subroutine get_small_protein


!-------------------------------------------------------------------------------
!   reassign_pqr_variables: if NTER is smallest, swap all variables around - 
!   e.g coords1 -> coords2, coords2 -> coords1
!   if CTER is smallest do nothing.    
!-------------------------------------------------------------------------------   

    subroutine reassign_pqr_variables(small_n_or_c,&
         atomtype1, coords1, atom1, restype1, resid1, chain1, n_atoms1, o1, b1, &
      atomtype2, coords2, atom2, restype2, resid2, chain2, n_atoms2, o2, b2)

      character(1), intent(in) :: small_n_or_c
      integer, dimension(:), allocatable, intent(inout) :: resid1, resid2 
      real, dimension(:,:), allocatable, intent(inout) :: coords1, coords2
      real, dimension(:), allocatable, intent(inout) :: o1, b1, o2, b2
      character(4), dimension(:), allocatable, intent(inout) :: atomtype1, atom1, restype1, chain1
      character(4), dimension(:), allocatable, intent(inout) :: atomtype2, atom2, restype2, chain2
      integer, intent(inout) :: n_atoms1, n_atoms2
      
      integer, dimension(size(resid1,1)) :: resid1t 
      real, dimension(size(coords1,1),size(coords1,2)) :: coords1t
      real, dimension(size(o1,1)) :: o1t, b1t
      character(4), dimension(size(atomtype1,1)) :: atomtype1t, atom1t, restype1t, chain1t

      integer, dimension(size(resid2,1)) :: resid2t 
      real, dimension(size(coords2,1),size(coords2,2)) :: coords2t
      real, dimension(size(o2,1)) :: o2t, b2t
      character(4), dimension(size(atomtype2,1)) :: atomtype2t, atom2t, restype2t, chain2t

      
      ! If NTER is smallest, then swap everything. If CTER is smallest, do nothing - program
      ! assumes that input has smallest CTER

      if (small_n_or_c == "c") then
         resid1t = resid1
         coords1t = coords1
         o1t = o1
         b1t = b1
         atomtype1t = atomtype1
         atom1t = atom1
         restype1t = restype1
         chain1t = chain1 

         resid2t = resid2
         coords2t = coords2
         o2t = o2
         b2t = b2
         atomtype2t = atomtype2
         atom2t = atom2
         restype2t = restype2
         chain2t = chain2 

         ! deallocate all arrays
         deallocate(resid1, coords1, o1, b1, atomtype1, atom1, restype1, chain1,&
              resid2, coords2, o2, b2, atomtype2, atom2, restype2, chain2)
         
         ! allocate new arrays
         allocate(resid1(size(resid2t,1)))
         allocate(coords1(size(coords2t,1),size(coords2t,2)))
         allocate(o1(size(o2t,1)))
         allocate(b1(size(b2t,1)))
         allocate(atomtype1(size(atomtype2t,1)))
         allocate(atom1(size(atom2t,1)))
         allocate(restype1(size(restype2t,1)))
         allocate(chain1(size(chain2t,1)))

         allocate(resid2(size(resid1t,1)))
         allocate(coords2(size(coords1t,1),size(coords1t,2)))
         allocate(o2(size(o1t,1)))
         allocate(b2(size(b1t,1)))
         allocate(atomtype2(size(atomtype1t,1)))
         allocate(atom2(size(atom1t,1)))
         allocate(restype2(size(restype1t,1)))
         allocate(chain2(size(chain1t,1)))

         ! Swap values
         resid1 = resid2t
         coords1 = coords2t
         o1 = o2t
         b1 = b2t
         atomtype1 = atomtype2t
         atom1 = atom2t
         restype1 = restype2t
         chain1 = chain2t

         resid2 = resid1t
         coords2 = coords1t
         o2 = o1t
         b2 = b1t
         atomtype2 = atomtype1t
         atom2 = atom1t
         restype2 = restype1t
         chain2 = chain1t   

         n_atoms1 = size(coords1,1)
         n_atoms2 = size(coords2,1)
         
      end if
         
      
      
    end subroutine reassign_pqr_variables


    
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
!   translation_matrix_check: 
!   
!-------------------------------------------------------------------------------
    subroutine translation_matrix_check(theta, phi, gamma, n_theta_rotations, n_phi_rotations, n_gamma_rotations, &
         unique, array_of_rot_arrays)
      
       double precision, intent(in) :: theta, phi, gamma
       integer, intent(in) :: n_theta_rotations, n_phi_rotations, n_gamma_rotations 
       logical, dimension(:,:,:), allocatable, intent(out) :: unique
       double precision, dimension(:,:,:,:,:), allocatable, intent(out) :: array_of_rot_arrays
       
       integer :: a, b, c, d, e, f
       double precision, dimension(3,3) :: theta_rot_mat, phi_rot_mat, gamma_rot_mat
       logical :: equal

       allocate(unique(n_theta_rotations,n_phi_rotations,n_gamma_rotations))
       allocate(array_of_rot_arrays(n_theta_rotations,n_phi_rotations,n_gamma_rotations,3,3))
       
       unique=.TRUE. 
      
       ! generate a 3x3 matrix with all the rotation matrices as entries.. so a (n_a, n_b, n_c, 3, 3) array
       do a=1, n_theta_rotations
          do b=1, n_phi_rotations
             do c=1, n_gamma_rotations
                call rotate((a-1)*theta, 1, theta_rot_mat)
                call rotate((b-1)*phi, 2, phi_rot_mat)
                call rotate((c-1)*gamma, 3, gamma_rot_mat)
                array_of_rot_arrays(a,b,c,:,:) = matmul(matmul(theta_rot_mat, phi_rot_mat), gamma_rot_mat)
             end do
          end do
       end do

       ! Check if they're equal
       do a=1, n_theta_rotations
          do b=1, n_phi_rotations
             
             do c=1, n_gamma_rotations
                if (unique(a,b,c) .eqv. .TRUE.) then 
                   
                   do d=1, n_theta_rotations
                      if (d .ne. a) then 
                         
                         do e=1, n_phi_rotations
                            if (e .ne. b) then 
                               
                               do f=1, n_gamma_rotations
                                  if (f .ne. c) then 
                                    
                                     call are_matrices_equal(array_of_rot_arrays(a,b,c,:,:), array_of_rot_arrays(d,e,f,:,:), equal) 
                                     if (equal .eqv. .TRUE.) then
                                        unique(d,e,f) = .FALSE.
                                     end if

                                  end if
                               end do
                               
                            end if
                         end do
                         
                      end if
                   end do
                   
                end if
             end do
             
          end do
       end do
          
     end subroutine translation_matrix_check
    
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
!   is_no_overlap: read as is_no_overlap?
!   checks across all coordinates of each protein to see if any distance 
!   between atoms is less than some cutoff distance.
!   If there is no overlap, return true
!   if overlap, return false
!   
!   
!-------------------------------------------------------------------------------

    subroutine is_no_overlap(coords1, coords2, overlap_dist, no_overlap)
      real, dimension(:,:), intent(in) :: coords1, coords2
      real, intent(in) :: overlap_dist
      logical, intent(out) :: no_overlap

      integer :: i,j
      real :: overlap_check_start, overlap_check_finish


      call cpu_time(overlap_check_start)
      no_overlap = .TRUE. 

      ! Loop over all coords in protein 1, then protein 2
      !$omp parallel do private(j)
      loop1: do i=1, size(coords1,1)
         do j=1, size(coords2,1)
            
            ! Check if distance between atoms in the two proteins is less than
            ! cutoff distance. If yes, is_no_overlap=false, exit from loops.
            ! If no overlap, is_no_overlap stays as true. 
            if (distance(coords1(i,:),coords2(j,:)) .lt. overlap_dist) then 
               no_overlap = .FALSE.
               exit loop1
            end if

         end do
       end do loop1
       !$omp end parallel do

      call cpu_time(overlap_check_finish)
!      print*, "Overlap check time: ", overlap_check_finish - overlap_check_start

    end subroutine is_no_overlap

!-------------------------------------------------------------------------------
!   within_cutoff_distance: read as within_cutoff_distance?
!   checks to see if NTER N atom and CTER C atom are within cutoff distance
!   of one another
!   Within cutoff, return true
!   not within cutoff, return false
!   
!   
!-------------------------------------------------------------------------------

    subroutine within_cutoff_distance(atom_coord1, atom_coord2, cutoff_dist, within_cutoff)
      real, dimension(3) :: atom_coord1, atom_coord2
      real, intent(in) :: cutoff_dist
      logical, intent(out) :: within_cutoff

      within_cutoff = .TRUE.

      if (distance(atom_coord1, atom_coord2) .gt. cutoff_dist) then
         within_cutoff = .FALSE.
      end if

    end subroutine within_cutoff_distance

!-------------------------------------------------------------------------------
!   within_exp_constraints: read as within_exp_constraints?
!   checks to see if indexes are within distance +/-t olerance
!   Within distance, return true
!   not within distance, return false
!   
!   
!-------------------------------------------------------------------------------

    subroutine within_exp_constraints(exp_constraints, coords1, coords2, small_n_or_c, valid_exp_constraints)
      real, dimension(:,:), intent(in) :: exp_constraints
      real, dimension(:,:), intent(in) :: coords1, coords2
      logical, intent(out) :: valid_exp_constraints
      character*1, intent(in) :: small_n_or_c
      
      integer :: index1, index2, i
      real :: min, max, dist
      real, dimension(3) :: atomic_coords1, atomic_coords2
      
      valid_exp_constraints = .TRUE.

      do i=1, size(exp_constraints,1)

         ! WARNING
         ! I THINK THE BELOW IS RIGHT, BUT IM  NOT ENTIRELY SURE
         ! IM TOO TIRED TO MAKE SENSE OF IT
         ! Make sure that atomic indexes point at correct protein
         ! if CTER is smaller protein, leave contraint setup as is in input file
         if (small_n_or_c == "n") then 
            index1 = nint(exp_constraints(i,1))
            index2 = nint(exp_constraints(i,2))
         ! if NTER is smaller protein, swap indexes around, as atom coords are swapped elsewhere.  
         elseif (small_n_or_c == "c") then
            index2 = nint(exp_constraints(i,1))
            index1 = nint(exp_constraints(i,2))
         else
            write(ERROR_UNIT,*) "ERROR: Somethings funking in subroutine: within_exp_constraints in md_mod.f90"
         end if
         
         atomic_coords1 = coords1(index1,:)
         atomic_coords2 = coords2(index2,:)
         dist = distance(atomic_coords1,atomic_coords2)
         min = exp_constraints(i,3)-exp_constraints(i,4)
         max = exp_constraints(i,3)+exp_constraints(i,4)
         
         if ((dist .lt. min) .or. (dist .gt. max)) then 
            valid_exp_constraints = .FALSE.
            exit
         end if
         
      end do


    end subroutine within_exp_constraints


!-------------------------------------------------------------------------------
!   is_coords_unique:
!   Different combinations of angles can produce the same coordinates. is_coords_unique
!   checks current set of coords against an array of saved unique coordinate arrays 
!   to see if it mathces any. If no match, current array is unique, and output  
!   is TRUE. If there is a match, array is not unique, output is FALSE.
!   
!-------------------------------------------------------------------------------

    subroutine is_coords_unique(coords, unique_coords, n_unique_coords, is_unique)
      real, dimension(:,:), intent(in) :: coords
      real, dimension(:,:,:), intent(inout) :: unique_coords
      integer, intent(inout) :: n_unique_coords
      logical, intent(out) :: is_unique

      ! unique_coords(n_atoms,3,10)
      
      integer :: i, j, k, max_check
      real :: tol 
      logical, dimension(:), allocatable :: is_coords_set_unique

      is_unique = .FALSE.
      ! Because of stupid real maths I need to add a tolerance in here
      ! Values of xyz coordinates can be 'identical' but differ in the
      ! third decimal place by 1.. so 0.001. But I'm going to be a bit
      ! hamfisted and increased the tolerance a bit more, in case there
      ! are another other larger straglers.
      tol = 0.01

      ! Set n_unique_coords to 0 on first call
      if ( all(unique_coords(:,:,1)==0) ) then
         n_unique_coords = 0
      end if

      ! Set the number of set of coords to check over
      ! We should only store a certain number of coord arrays in
      ! unique_coords.. I'm worried about memory usage. This is
      ! defined in the program with an allocate statement. If
      ! the number of unique coordinates found is less than this
      ! we only need to check over this number, as checking over the
      ! empty (0) arrays will show everything up to size(unique_coords,3)
      ! is unique. And we can't have that. Can we now? 
      if ( n_unique_coords .lt. size(unique_coords,3) ) then
         max_check = n_unique_coords
      else
         max_check = size(unique_coords,3)
      end if

      ! is_coords_set_unique is an 1-D logical array of size max_check
      ! (number of arrays to check over). An entry is true if the current
      ! tested coords are unique against the ith set of coords in
      ! unique_coords. It enters False if current coords are identical
      ! to ith set of coords. All entries need to be TRUE for the current
      ! set of coords to be declared unique.
      allocate(is_coords_set_unique(max_check))
      
      ! If this is the first time, just add coords into unique_coords
      if (n_unique_coords == 0) then
         unique_coords(:,:,1) = coords
         n_unique_coords = n_unique_coords + 1
         is_unique = .TRUE.
      else
         ! Check over last 10 unique_coords
         do i=1, max_check
            is_coords_set_unique(i) = .FALSE.
            
            loop1: do j=1, size(coords,1)

               ! If current xyz match reference xyz, then cool.. go to next xyz
               ! until we run out of atomsto look at. If all match, then 
               ! is_coords_set_unique(i) remains false.
               if ( (abs(coords(j,1) - unique_coords(j,1,i)) .lt. tol) .and. &
                    (abs(coords(j,2) - unique_coords(j,2,i)) .lt. tol) .and. &
                    (abs(coords(j,3) - unique_coords(j,3,i)) .lt. tol) ) then 
                  continue
               else
                  ! If one of the current xyz coords do not match ref xyz coords,
                  ! then current coords are unique. Set is_coords_set_unique(i)
                  ! to true
                  is_coords_set_unique(i) = .TRUE.
                  exit loop1
               end if
            end do loop1
         end do

         ! Check over all the TRUE/FALSE entries for each saved array to see if new coords
         ! is unique

         ! If current coords are unique against last $max_check coords then set is_unique
         ! to true and update unique coords array
         if ( all(is_coords_set_unique .eqv. .TRUE.) ) then
            is_unique=.TRUE.
            n_unique_coords = n_unique_coords + 1

            ! Shift unique coords along 1 in unique_coords array
            do k=size(unique_coords,3)-1,1,-1
               unique_coords(:,:,k+1) = unique_coords(:,:,k)
            end do
            ! Add current coords into unique coords array
            unique_coords(:,:,1) = coords
         else
            ! If current coords matches at least one of previous coords,
            ! is_unique is false
            is_unique=.FALSE.
         end if
      end if

    end subroutine is_coords_unique

    
!-------------------------------------------------------------------------------
!   GENERATE_PROTEIN_COMBINATION: takes coordinates, performs translation and
!   rotation returns new coordinates. 
!
!
!-------------------------------------------------------------------------------
    subroutine generate_protein_coords(coords, resid, atomtype, translate_vector, rot_point, theta, phi, rrt_coords)
      ! Theta is rotation in x, phi is rotation in y
      real, dimension(:,:), intent(in) :: coords
      integer, dimension(:), intent(in) :: resid
      character(4), dimension(:), intent(in) :: atomtype
      real, dimension(3), intent(in) :: translate_vector, rot_point
      double precision, intent(in) :: theta, phi
      real, dimension(:,:), allocatable, intent(out) :: rrt_coords
      
      real, dimension(:,:), allocatable :: t_coords, rt_coords

      ! Assume protein being moved around is second protein, to which the linker is
      ! connected to the NTER. 
      call translate_coords(coords, translate_vector, t_coords)
      call rotate_coords(t_coords, rot_point, theta, 1, rt_coords)
      call rotate_coords(rt_coords, rot_point, phi, 2, rrt_coords)

    end subroutine generate_protein_coords


!-------------------------------------------------------------------------------
!   check_coords_are_valid: uses distance and overlap checks to see if new
!   coords are valid and suitable for output. Returns true if valid coordinates
!   returns false if not valid coordinates.     
!
!   Assumes coords are protein with linker connecting to NTER (second protein)
!   Assumes ref_coords are protein with linker connecting to CTER (first protein)    
!-------------------------------------------------------------------------------  

    subroutine check_coords_are_valid(coords, resid, atomtype, ref_coords, ref_resid, ref_atomtype,&
      exp_constraints, cutoff_dist, overlap_dist, small_n_or_c, big_n_or_c, is_coords_valid)
      ! Ref is large protein
      
      real, dimension(:,:), intent(in) :: coords, ref_coords, exp_constraints
      integer, dimension(:), intent(in) :: resid, ref_resid
      character(4), dimension(:), intent(in) :: atomtype, ref_atomtype
      logical, intent(out) :: is_coords_valid
      real, intent(in) :: cutoff_dist, overlap_dist
      character(1), intent(in) :: small_n_or_c, big_n_or_c
      
      real, dimension(3) :: small_TER_coords, big_TER_coords
      logical :: within_cutoff, no_overlap, valid_exp_constraints

      ! Have to demonstrate new coordinates are valid
      is_coords_valid = .FALSE.
     
      ! Get link
      call get_TER_coords(coords, resid, atomtype, small_n_or_c, small_TER_coords)
      call get_TER_coords(ref_coords, ref_resid, ref_atomtype, big_n_or_c, big_TER_coords)

      ! Only print file if distance between linker bases are within cutoff
      ! and if there is no overlap of the two proteins

      ! Run checks
      call within_cutoff_distance(small_TER_coords, big_TER_coords, cutoff_dist, within_cutoff)     
      call within_exp_constraints(exp_constraints, coords, ref_coords, small_n_or_c, valid_exp_constraints)
      call is_no_overlap(coords, ref_coords, overlap_dist, no_overlap)


      ! See if validation critera is met. If met, return True. 
      if ((within_cutoff .eqv. .TRUE.) .and. (no_overlap .eqv. .TRUE.)&
           .and. (valid_exp_constraints .eqv. .TRUE.)) then 
         is_coords_valid = .TRUE.
      end if

      end subroutine check_coords_are_valid


!-------------------------------------------------------------------------------
!   generate_output_filename: takes translation vector and angles 
!   to create a unique name for the output pqr file. 
!-------------------------------------------------------------------------------
    subroutine generate_output_filename(trans_vector, theta, phi, gamma, output_filename)
      real, dimension(3), intent(in) :: trans_vector
      double precision, intent(in) :: theta, phi, gamma
      character*80, intent(out) :: output_filename

      character*80 :: ctv1, ctv2, ctv3, ctheta, cphi, cgamma, cdist
      real :: theta_deg, phi_deg, gamma_deg, dist
      
      ! Round trans_vector, theta and phi
      ! to one decimal place
      ! r = rounded

      theta_deg = (theta * 180)/PI
      phi_deg = (phi * 180)/PI
      gamma_deg = (gamma * 180)/PI

      dist = mag(trans_vector)
      
      write(cdist,'(F6.1)') dist
      write(ctv1,'(F6.1)') trans_vector(1)
      write(ctv2,'(F6.1)') trans_vector(2)
      write(ctv3,'(F6.1)') trans_vector(3) 
      write(ctheta,'(F6.1)') theta_deg
      write(cphi,'(F6.1)')  phi_deg
      write(cgamma,'(F6.1)')  gamma_deg 
    
      ! Combine to form output filename with .pqr extension
      output_filename =  trim(adjustl(cdist)) // '_' // &
           trim(adjustl(ctv1)) // '_' // trim(adjustl(ctv2)) // '_' // trim(adjustl(ctv3)) //&
           '_' // trim(adjustl(ctheta)) // '_' // trim(adjustl(cphi)) &
           // '_' // trim(adjustl(cphi)) //'.pdb'

    end subroutine generate_output_filename


!-------------------------------------------------------------------------------
!   generate_logfile_filename: takes pqr file names, smooshes them together
!   makes a new filename
!-------------------------------------------------------------------------------
    subroutine generate_log_filename(pqr_filename1, pqr_filename2, log_filename)
      character*80, intent(in) :: pqr_filename1, pqr_filename2
      character*80, intent(out) :: log_filename

      character*80 :: prefix1, prefix2 
      integer :: pos1, pos2

      ! Get position of full stop of each pqr file
      pos1 = scan(trim(pqr_filename1),".", BACK= .true.)
      pos2 = scan(trim(pqr_filename2),".", BACK= .true.)

      ! Define prefix of filename as everything before full stop
      if ( pos1 > 0 ) prefix1 = pqr_filename1(1:pos1-1)
      if ( pos2 > 0 ) prefix2 = pqr_filename2(1:pos2-1)
      
      ! Combine prefixes to form output filename with .log extension
      log_filename = trim(adjustl(prefix1)) // '_' // trim(adjustl(prefix2)) // '.log'
      
    end subroutine generate_log_filename


!-------------------------------------------------------------------------------
!   write_valid_log_file: write stats about conformations found to be valid
!    
!-------------------------------------------------------------------------------
    subroutine write_log_file_header(log_unit, conf_filename, pqr_filename1, pqr_filename2, cutoff_dist, &
         grid_spacing, theta, phi, gamma, overlap_dist, exp_constraints)

      integer, intent(in) :: log_unit
      character*80, intent(in) :: conf_filename, pqr_filename1, pqr_filename2
      real, intent(in) :: cutoff_dist, overlap_dist, grid_spacing      
      double precision, intent(in) :: theta, phi, gamma
      real, dimension(:,:), intent(in) :: exp_constraints
      
      double precision :: theta_deg, phi_deg, gamma_deg
      integer :: i

      theta_deg = (theta * 180)/PI
      phi_deg = (phi * 180)/PI
      gamma_deg = (gamma * 180)/PI
      
      write(log_unit,*) "######################### BloBerator #########################"
      write(log_unit,*) "Written by Michael Thomas for Macka"
      write(log_unit,*) "June 2016"
      write(log_unit,*) "Australian National University"
      write(log_unit,*) "Questions?: email michael.thomas@anu.edu.au"
      write(log_unit,*) ""
      write(log_unit,*) ""
      write(log_unit,*) "###################### Input Paramaters ######################"
      write(log_unit,*) "Input file: ", trim(adjustl(conf_filename))
      write(log_unit,*) "Protein with CTER linker: ", trim(adjustl(pqr_filename1))
      write(log_unit,*) "Protein with NTER linker: ",trim(adjustl(pqr_filename2))
      write(log_unit,*) "Linker length:", cutoff_dist, "Angstroms"
      write(log_unit,*) "Grid spacing:", grid_spacing, "Angstroms"
      write(log_unit,*) "Angle of rotation about x axis: ", theta_deg, "degrees"
      write(log_unit,*) "Angle of rotation about y axis: ", phi_deg, "degrees"
      write(log_unit,*) "Angle of rotation about z axis: ", gamma_deg, "degrees"
      write(log_unit,*) "vDW distance for overlap criteria: ", overlap_dist, "Angstroms"
      write(log_unit,*) "Number of experimental constraints: ", size(exp_constraints,1)
      if (size(exp_constraints,1) .gt. 0) then
         write(log_unit,*) "Atom index 1     Atom index 2   Constraint Distance   Constraint Tolerance"
         do i=1, size(exp_constraints,1)
            write(log_unit,*) exp_constraints(i,:)
         end do
      end if
      write(log_unit,*) ""
      write(log_unit,*) ""
      write(log_unit,*) "################ Valid Protein Conformations ################"
      write(log_unit,*) "Distance (A)       Theta (degrees)           Phi (degre&
           es)             Gamma(degrees)          Coordinate Filename"
    end subroutine write_log_file_header
    
    
    
!-------------------------------------------------------------------------------
!   write_valid_log_file: write stats about conformations found to be valid
!    
!-------------------------------------------------------------------------------
    subroutine write_valid_to_log_file(log_unit, dist, theta, phi, gamma, output_pqr_filename)

      integer, intent(in) :: log_unit
      real, intent(in) :: dist
      double precision, intent(in) :: theta, phi, gamma 
      character*80, intent(in) :: output_pqr_filename
      
      double precision :: theta_deg, phi_deg, gamma_deg
      
      theta_deg = (theta * 180)/PI
      phi_deg = (phi * 180)/PI
      gamma_deg = (gamma * 180)/PI
      
      write(log_unit,*) dist, theta_deg, phi_deg, gamma_deg, trim(adjustl(output_pqr_filename))
      
    end subroutine write_valid_to_log_file

!-------------------------------------------------------------------------------
!   write_log_file_summary: print some stats to end of log file
!    
!-------------------------------------------------------------------------------
    subroutine write_log_file_summary(log_unit, n_valid_configs, n_total_configs)

      integer, intent(in) :: n_valid_configs, n_total_configs, log_unit
      character*80 :: c_nvc, c_ntc, c_per

      write(c_nvc,*) n_valid_configs
      write(c_ntc,*) n_total_configs
      write(c_per,*) 100*(real(n_valid_configs)/real(n_total_configs))

      write(log_unit,*) trim(adjustl(c_nvc)), " valid protein configurations"
      write(log_unit,*) trim(adjustl(c_ntc)), " total configurations tested"
      write(log_unit,*) trim(adjustl(c_per)), "%"
      
    end subroutine write_log_file_summary

!-------------------------------------------------------------------------------
! split a string into 2 either side of a delimiter token
! taken from https://gist.github.com/ponderomotion/3833266
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
