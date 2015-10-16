---
title: Enable Linux USB 2.0 ports for Sony Vaio T15 
author: Nikolaos Bezirgiannis
tags: linux, kernel, sony, vaio
---

My current setup at work includes
a [15-inch Sony Vaio T series](http://www.notebookcheck.net/Review-Sony-Vaio-SV-T1511M1E-S-Ultrabook.89212.0.html) (Product number SVT1511C5E)
running a fresh [ArchLinux](http://archlinux.org) x86_64. Whereas my laptop's USB3.0 port
works perfectly fine and at full speed, the other two USB2.0 ports
are not functioning at all. Specifically, any connected device
to these ports will normally power up (charge), but the Linux kernel will not recognize and mount them.
I tested it also with two versions of Ubuntu (13.04 and 13.10), but the problem
seems to persist; note that the USB2.0 ports work fine under Windows 8.0/8.1.

I guess this is a problem with other Linux distros as well, so I thought to 
provide you with a general solution I constructed, after spending many hours on googling. 
Lastly, this solution might be applicable for other similar VAIO laptops; 
I guess Sony VAIO laptops in the T series (e.g. the smaller T13) and Pro series are also affected.

I stumbled on this [Launchpad bug thread](https://bugs.launchpad.net/ubuntu/+source/linux/+bug/1210858),
where they confirm the problem in many other distros and Linux kernel versions.
The user *Combat* in the thread provided a kernel patch (<https://bugs.launchpad.net/ubuntu/+source/linux/+bug/1210858/comments/32>)
that allegedly works. The patch is targeted for the Ubuntu stock kernel and is written for the now outdated Linux 3.11 version.
I since rewrote the patch for the recent kernel version (*at the time of the writing it was 3.14*) and port it over to ArchLinux.

The porting to the recent kernel version was proven to be a fairly straightforward process. I was amazed, however, by how much the source has changed over such minor revisions of the kernel (3.11->3.14). I then rebuilt the kernel
to test Combat's patch and, after a reboot both USB2.0 ports were operational!
I was fairly excited about the result and was planning to publish my custom
linux kernel build in [AUR](https://aur.archlinux.org/) for other VAIO users to enjoy.
Unfortunately, after some further testing, I realised that after my laptop
was woken up (resumed) from a **suspension**, the USB2.0 ports stopped working again; a full reboot was the only way to bring them back.

Googling this time did not bring any results for this strange case, so I decided to dig up in the kernel source tree.
Next to the source file that Combat had patched, I found a related file that
looked to be the culprit, since it was responsible for resuming the USB xHCI host controller after a suspension. 
Following my intuition, I modified this source file according to the previous patch and rebuilt the kernel.
After thorough testing of the new custom kernel, thet USB2.0 ports remain working after multiple resumptions!

As promised, I packaged the solution in a PKGBUILD, so other ArchLinux users can build the custom kernel.
I will try to keep it up-to-date with the stock kernel. The build can be found in AUR under the package name [linux-vaio-usb](https://aur.archlinux.org/packages/linux-vaio-usb/). Also, you can find on AUR the accompanying [linux-vaio-usb-headers](https://aur.archlinux.org/packages/linux-vaio-usb-headers/) and [linux-vaio-usb-docs](https://aur.archlinux.org/packages/linux-vaio-usb-docs/) packages.

I include the final patch because it may be used for other distros as well, considering that the stock ArchLinux kernel does not much deviate from the Linux upstream.

### Patch

~~~ {.diff}
--- a/drivers/usb/host/pci-quirks.c
+++ b/drivers/usb/host/pci-quirks.c
@@ -997,8 +997,8 @@ static void quirk_usb_handoff_xhci(struc
 	writel(val, base + ext_cap_offset + XHCI_LEGACY_CONTROL_OFFSET);
 
 hc_init:
-	if (pdev->vendor == PCI_VENDOR_ID_INTEL)
-		usb_enable_intel_xhci_ports(pdev);
+	/* if (pdev->vendor == PCI_VENDOR_ID_INTEL) */
+	/* 	usb_enable_intel_xhci_ports(pdev); */
 
 	op_reg_base = base + XHCI_HC_LENGTH(readl(base));
 
--- a/drivers/usb/host/xhci-pci.c
+++ b/drivers/usb/host/xhci-pci.c
@@ -304,8 +304,8 @@ static int xhci_pci_resume(struct usb_hc
 	 * xHCI host controllers) have been resumed.
 	 */
 
-	if (pdev->vendor == PCI_VENDOR_ID_INTEL)
-		usb_enable_intel_xhci_ports(pdev);
+	/* if (pdev->vendor == PCI_VENDOR_ID_INTEL) */
+	/* 	usb_enable_intel_xhci_ports(pdev); */
 
 	retval = xhci_resume(xhci, hibernated);
 	return retval;
~~~
