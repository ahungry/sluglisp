`cl-virtualbox` is a library that allows you to control VirtualBox from Common
Lisp, by calling the `vboxmanage` command.

# Usage

## Listing VMs

* `list-vms ()`: Return a list of plists with the :name and :uuid of every
  virtual machine.

* `list-running-vms ()`: Like `list-vms`, but only return the VMs that are
  running.

Examples:

~~~lisp
virtualbox> (list-vms)
((:uuid "68fdde1a-a009-4c6f-af6e-250ef879e3e7" :name
  "crane_ubuntu_1410013141185_11153")
 (:uuid "b0ebec92-85cb-408c-b32c-c969e6392d5f" :name
  "vm_ubuntu_1410966111913_95217")
 (:uuid "9ad98045-5109-4233-8b82-4c9a49b4cab7" :name
  "trivial-ssh_default_1411575480546_50302")
 (:uuid "5d287f3c-9b17-4a66-990a-80d4db3167ae" :name "windows"))
virtualbox> (list-running-vms)
nil
~~~

## Finding VMs

* `find-by-name (name)`: Find a virtual machine by name.

* `find-by-uuid (uuid)`: Find a virtual machine by UUID.

Examples:

~~~lisp
virtualbox> (find-by-name "crane_ubuntu_1410013141185_11153")
(:uuid "68fdde1a-a009-4c6f-af6e-250ef879e3e7" :name
 "crane_ubuntu_1410013141185_11153")
virtualbox> (find-by-uuid "9ad98045-5109-4233-8b82-4c9a49b4cab7")
(:uuid "9ad98045-5109-4233-8b82-4c9a49b4cab7" :name
 "trivial-ssh_default_1411575480546_50302")
~~~

## Creating and Modifying VMs

* `create-vm (name)`: Create a new virtual machine named `name`.

* `set-vm-memory (name memory)`: Set the VM's memory (In megabytes)

* `set-vm-vram (name memory)`: The the VM's video memory (In megabytes).

* `set-vm-cpu-count (name count)`: Set the number of virtual CPUs the VM has.

* `set-vm-acpi (name state)`: Turn ACPI support on/off.

* `set-vm-ioapic (name state)`: Turn IOAPIC support on/off.

* `set-vm-pae (name state)`: Enable/disable PAE.

* `set-vm-longmode (name state)`: Enable/disable longmode.

* `set-vm-hpet (name state)`: Enable/disable the High-Precision Event Timer
  (HPET).

* `set-vm-3d-acceleration (name state)`: Enable/disable 3D acceleration.

## Network Configuration

* `map-vm-ports (name host-port guest-port)`: Map TCP traffic from `host-port`
  to `guest-ip:guest-port` in the guest.

## Controlling VM State

* `start-vm (name &key (type headless))`: Start the virtual machine.

* `pause-vm (name)`: Pause the virtual machine.

* `resume-vm (name)`: Resume the virtual machine after pausing it.

* `cold-reboot-vm (name)`: Reboot the virtual machine.

* `poweroff-vm (name)`: Power off the virtual machine.

## Managing Hard Drives

* `create-hd (path &key size (format vdi))`: Create a virtual hard drive on
  `path`, with size `size` (In megabytes) and type `type` (:vdi by default).

## Managing DVDs

* `mount-dvd (name path)`: Mount a DVD to the virtual machine.

* `unmount-dvd (name)`: Remove the DVD from the virtual DVD drive.

# License

Copyright (c) 2014 Fernando Borretti (eudoxiahp@gmail.com)

Licensed under the MIT License.
