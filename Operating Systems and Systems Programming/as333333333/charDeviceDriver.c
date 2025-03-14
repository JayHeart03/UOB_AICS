/*
 *  chardev.c: Creates a read-only char device that says how many times
 *  you've read from the dev file
 */

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <asm/uaccess.h> /* for put_user */
#include <charDeviceDriver.h>

#include <linux/init.h>
#include <linux/list.h>
#include <linux/slab.h>
#include "ioctl.h"

MODULE_LICENSE("GPL");

/*
 * This function is called whenever a process tries to do an ioctl on our
 * device file. We get two extra parameters (additional to the inode and file
 * structures, which all device functions get): the number of the ioctl called
 * and the parameter given to the ioctl function.
 *
 * If the ioctl is write or read/write (meaning output is returned to the
 * calling process), the ioctl call returns the output of this function.
 *
 */

DEFINE_MUTEX(devLock);
DEFINE_MUTEX(rwLock);

static int counter = 0;
static struct list_head listhead;

static long device_ioctl(struct file *file,		 /* see include/linux/fs.h */
						 unsigned int ioctl_num, /* number and param for ioctl */
						 unsigned long ioctl_param)
{
	/*
	 * Switch according to the ioctl called
	 */
	if (ioctl_num == RESET_COUNTER)
	{
		counter = 0;
		/* 	    return 0; */
		return 5; /* can pass integer as return value */
	}

	else
	{
		/* no operation defined - return failure */
		return -EINVAL;
	}
}

/*
 * This function is called when the module is loaded
 */
int init_module(void)
{
	Major = register_chrdev(0, DEVICE_NAME, &fops);

	if (Major < 0)
	{
		printk(KERN_ALERT "Registering char device failed with %d\n", Major);
		return Major;
	}

	printk(KERN_INFO "I was assigned major number %d. To talk to\n", Major);
	printk(KERN_INFO "the driver, create a dev file with\n");
	printk(KERN_INFO "'mknod /dev/%s c %d 0'.\n", DEVICE_NAME, Major);
	printk(KERN_INFO "Try various minor numbers. Try to cat and echo to\n");
	printk(KERN_INFO "the device file.\n");
	printk(KERN_INFO "Remove the device file and module when done.\n");

	INIT_LIST_HEAD(&listhead);

	return SUCCESS;
}

/*
 * This function is called when the module is unloaded
 */
// Removing the module deallocates all messages, removes the list of messages and removes the device.
// removes the all list of messages

void cleanup_module(void)
{
	struct msg_struct *p, *temp;

	list_for_each_entry_safe(p, temp, &listhead, list)
	{
		list_del(&p->list);
		kfree(p);
	}

	unregister_chrdev(Major, DEVICE_NAME);
}
/*
 * Methods
 */

/*
 * Called when a process tries to open the device file, like
 * "cat /dev/mycharfile"
 */
static int device_open(struct inode *inode, struct file *file)
{

	mutex_lock(&devLock);
	if (Device_Open)
	{
		mutex_unlock(&devLock);
		return -EBUSY;
	}
	Device_Open++;
	mutex_unlock(&devLock);
	sprintf(msg, "I already told you %d times Hello world!\n", counter++);
	try_module_get(THIS_MODULE);

	return SUCCESS;
}

static int device_release(struct inode *inode, struct file *file)
{
	mutex_lock(&devLock);
	Device_Open--; /* We're now ready for our next caller */
	mutex_unlock(&devLock);
	/*
	 * Decrement the usage count, or else once you opened the file, you'll
	 * never get get rid of the module.
	 */
	module_put(THIS_MODULE);

	return 0;
}

/*
 * Called when a process, which already opened the dev file, attempts to
 * read from it.
 */
// Reading from the device returns one message, and removes this message from the kernel list.

// If the list of messages is empty, the reader returns -EAGAIN.

static ssize_t device_read(struct file *filp, /* see include/linux/fs.h   */
						   char *buffer,	  /* buffer to fill with data */
						   size_t length,	  /* length of the buffer     */
						   loff_t *offset)
{
	struct msg_struct *temp;
	char *msg;
	int result;

	/* result of function calls */
	// read the massage from the divice
	/*
	 * Actually put the data into the buffer
	 */
	mutex_lock(&rwLock);
	if (list_empty(&listhead))
	{
		mutex_unlock(&rwLock);
		return -EAGAIN;
	}

	temp = list_first_entry(&listhead, struct msg_struct, list);
	msg = temp->masg;

	if (strlen(msg) + 1 < length)
		length = strlen(msg) + 1;
	result = copy_to_user(buffer, msg, length);
	if (result > 0)
	{
		mutex_unlock(&rwLock);
		return -EFAULT;
	}
	list_del(&temp->list);
	kfree(temp);
	mutex_unlock(&rwLock);

	/*
	 * Most read functions return the number of bytes put into the buffer
	 */
	return length;
}

/* Called when a process writes to dev file: echo "hi" > /dev/hello  */
// Writing to the device stores the message in kernel space and adds it to the list if the message is below the maximum size, and the limit of the number of all messages stored in the kernel  wouldnt be surpassed with this message. If the message is too big, -EINVAL is returned, and if the limit of the number of all messages was surpassed, -EBUSY is returned.

static ssize_t
device_write(struct file *filp, const char *buff, size_t len, loff_t *off)
{
	int result;
	struct msg_struct *temp;
	struct list_head *pos, *q;

	int MAX_MSG_NUM = 1000;
	counter = 0;

	mutex_lock(&rwLock);
	list_for_each_safe(pos, q, &listhead)
	{
		counter++;
	}
	if (len + 1 > BUF_LEN)
	{
		mutex_unlock(&rwLock);
		return -EINVAL;
	}
	if (counter >= MAX_MSG_NUM)
	{
		mutex_unlock(&rwLock);
		return -EBUSY;
	}
	temp = kmalloc(sizeof(struct msg_struct), GFP_KERNEL);
	result = copy_from_user(temp->masg, buff, len);
	if (result > 0)
	{
		mutex_unlock(&rwLock);
		return -EFAULT;
	}
	INIT_LIST_HEAD(&temp->list);
	list_add_tail(&temp->list, &listhead);
	mutex_unlock(&rwLock);

	return len;
}
