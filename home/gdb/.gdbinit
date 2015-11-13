define dmesg
        set $__log_buf = $arg0
        set $log_start = $arg1
        set $log_end = $arg2
        set $x = $log_start
        echo "
        while ($x < $log_end)
                set $c = (char)(($__log_buf)[$x++])
                printf "%c" , $c
        end
        echo "\n
end
document dmesg
dmesg __log_buf log_start log_end
Print the content of the kernel message buffer
end
#gdb implementation of the linux lsmod
define lsmod
        # The Linux kernel contains a generic double linked list implementation.
        # The "modules struct" is such a linked list and it contains information about modules.
 
        set $current = modules.next 

        #The list implementation is done by adding a list_head struct to a container
        #containing the data that is to be "listed". If one know the offset in bytes
        #between the start of the struct and the "list_head" a simple formular can 
        #be defined to determine the "content" of the list item. This value
        #is dependent on alignment and storage sized of the data in the struct
        #and is determined here for the struct module..

        set $container_offset =  ((int)&((struct module *)0).list) 

        #Iterate over the list printing modules information
        while($current != modules.prev)
                printf "%s 0x%08x\n",  \
                        ((struct module *) (((char*) ($current)) - $container_offset ) )->name ,\
                        ((struct module *) (((char*) ($current)) - $container_offset ) )->module_core
                set $current = $current.next 
        end
end
set pagination off

set history save on
set history size 10000
set history filename ~/.gdbhistory
set output-radix 16

add-auto-load-safe-path ~/projs
add-auto-load-safe-path /projects
