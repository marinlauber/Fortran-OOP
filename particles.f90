module particleMod
  implicit none
  type vector
    real :: p(3)
  end type vector
  type particle
     type(vector) :: position, velocity
     real :: mass
    contains
     procedure,public :: describe_particle, set
  end type particle
  type, extends(particle) :: charged_particle
     real :: charge
  end type charged_particle

 contains

  subroutine set(p,position,velocity,mass)
    implicit none
    class(particle),intent(inout) :: p
    real,intent(in) :: position(3),velocity(3),mass
    p%position%p = position
    p%velocity%p = velocity
    p%mass = mass
    select type (p)
    type is (charged_particle)
      p%charge = sum(p%position%p*p%mass)
    end select
  end subroutine set

  subroutine describe_particle(p)
    class(particle) :: p
    ! These attributes are common to all particles.
    print *, p%position%p
    print *, p%velocity%p
    print *, 'Mass:',p%mass
    ! Check for other attributes.
    select type (p)
      type is (charged_particle)
        print *,'Charge:',p%charge
      class is (charged_particle)
        print *,'Charge:',p%charge
        print *,'... may have other (unknown) attributes.'
      type is (particle)
        ! Just the basic particle type, there is nothing extra.
      class default
        print *,'... may have other (unknown) attributes.'
    end select
  end subroutine describe_particle

  subroutine test_particle
    implicit none
    type(particle) :: p
    type(charged_particle) :: cp
    call p%set([0.,0.,0.],[1.,0.,0.],12.)
    call p%describe_particle
    call cp%set(position=(/1.,1.,1./),velocity=(/0.,1.,0./),mass=1.)
    call cp%describe_particle
  end subroutine test_particle
end module particleMod
program main
  use particleMod
  call test_particle
end program main
