program main
    implicit none
    call testing
end program main

subroutine testing()
    use KindType
    use YAML_Node
    use YAML_Value
    implicit none
    !
    type(yaml_node_t), pointer :: root
    type(yaml_node_t), pointer :: child

    root => yaml_node_t('config')
    child => yaml_node_t('time',2)
    call root%add_child(child)
    child => yaml_node_t('space',2)
    call root%add_child(child)
    child => yaml_node_t('nt',4)
    child%value = yaml_value_t(3.3_rp)
    call root%first_child%add_child(child)
    child => yaml_node_t('nx',4)
    child%value = yaml_value_t(120_ip)
    call root%first_child%add_child(child)

    child => yaml_node_t('label',2)
    child%value = yaml_value_t('restart')
    call root%first_child%next_sibling%add_child(child)


    child => yaml_node_t('enabled',6)
    child%value = yaml_value_t(.true.)
    call root%first_child%add_child(child)

    child => yaml_node_t('lista',2)
    child%value = yaml_value_t(["hola", "nene", "si  "])
    call root%first_child%add_child(child)
    
    call root%print

    deallocate(root)

end subroutine testing



