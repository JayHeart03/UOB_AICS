/*
 *  chardev.c: Creates a read-only char device that says how many times
 *  you've read from the dev file
 */
/* Global definition for the example character device driver */

int init_module(void);
void cleanup_module(void);
static int device_open(struct inode *, struct file *);
static int device_release(struct inode *, struct file *);
static ssize_t device_read(struct file *, char *, size_t, loff_t *);
static ssize_t device_write(struct file *, const char *, size_t, loff_t *);
static long device_ioctl(struct file *file, unsigned int ioctl_num, unsigned long);

#define SUCCESS 0
#define DEVICE_NAME "chardev"	/* Dev name as it appears in /proc/devices   */
#define BUF_LEN 80		/* Max length of the message from the device */

/* 
 * Global variables are declared as static, so are global within the file. 
 */
struct cdev *my_cdev;
       dev_t dev_num;

static int Major;		/* Major number assigned to our device driver */
static int Device_Open = 0;	/* Is device open?  
				 * Used to prevent multiple access to device */
static char msg[BUF_LEN];	/* The msg the device will give when asked */



static struct file_operations fops = {
	.read = device_read,
	.write = device_write,
	.open = device_open,
	.unlocked_ioctl = device_ioctl,
	.release = device_release
};

struct msg_struct {
	struct list_head list;
	char msg[BUF_LEN];
};

#include <linux/kernel.h>
#include <linux/module.h>
#include <linux/fs.h>
#include <asm/uaccess.h>	/* for put_user */
#include <charDeviceDriver.h>
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




DEFINE_MUTEX  (devLock);
static int counter = 0;

static long device_ioctl(struct file *file,	/* see include/linux/fs.h */
		 unsigned int ioctl_num,	/* number and param for ioctl */
		 unsigned long ioctl_param)
{

	/* 
	 * Switch according to the ioctl called 
	 */
	if (ioctl_num == RESET_COUNTER) {
	    counter = 0; 
	    /* 	    return 0; */
	    return 5; /* can pass integer as return value */
	}

	else {
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
		static struct list_head list;
		INIT_LIST_HEAD(&list);

	if (Major < 0) {
	  printk(KERN_ALERT "Registering char device failed with %d\n", Major);
	  return Major;
	}

	printk(KERN_INFO "I was assigned major number %d. To talk to\n", Major);
	printk(KERN_INFO "the driver, create a dev file with\n");
	printk(KERN_INFO "'mknod /dev/%s c %d 0'.\n", DEVICE_NAME, Major);
	printk(KERN_INFO "Try various minor numbers. Try to cat and echo to\n");
	printk(KERN_INFO "the device file.\n");
	printk(KERN_INFO "Remove the device file and module when done.\n");

	return SUCCESS;
}


/*
 * This function is called when the module is unloaded
 */
void cleanup_module(void)
{
	/*  Unregister the device */
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
    
    mutex_lock (&devLock);
    if (Device_Open) {
	mutex_unlock (&devLock);
	return -EBUSY;
    }
    Device_Open++;
    mutex_unlock (&devLock);
    sprintf(msg, "I already told you %d times Hello world!\n", counter++);
    try_module_get(THIS_MODULE);
    
    return SUCCESS;
}

/* Called when a process closes the device file. */
Removing the module deallocates all messages, removes the list of messages and removes the device.

static int device_release(struct inode *inode, struct file *file)
{
	struct msg_struct *tmp;
	struct list_head *pos, *q;
	Device_Open--;		/* We're now ready for our next caller */
	/* 
	 * Decrement the usage count, or else once you opened the file, you'll
	 * never get get rid of the module. 
	 */
	module_put(THIS_MODULE);
	list_for_each_safe(pos, q, &list) {
		tmp = list_entry(pos, struct msg_struct, list);
		list_del(pos);
		kfree(tmp);
	}
	return 0;
}


static int device_release(struct inode *inode, struct file *file)
{
    mutex_lock (&devLock);
	Device_Open--;		/* We're now ready for our next caller */
	mutex_unlock (&devLock);
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
//Reading from the device returns one message, and removes this message from the kernel list. If the list of messages is empty, the reader returns -EAGAIN.
static ssize_t device_read(struct file *filp,	/* see include/linux/fs.h   */
			   char *buffer,	/* buffer to fill with data */
			   size_t length,	/* length of the buffer     */
			   loff_t * offset)
{{
	/* result of function calls */
	int result;
	/* number of bytes actually written to the buffer */
	int bytes_read = 0;
	/* pointer to the current message */
	struct msg_struct *tmp;
	/* pointer to the current position in the list */
	struct list_head *pos;
	/* 
	 * if we're at the end of the message, 
	 * return 0 signifying end of file 
	 */
	if (list_empty(&list)) {
		return -EAGAIN;
	}
	/* 
	 * get the first message in the list 
	 */
	pos = list.next;
	tmp = list_entry(pos, struct msg_struct, list);
	/* 
	 * copy the message to the buffer 
	 */
	while (length && tmp->msg[bytes_read]) {
		/* 
		 * the buffer is in the user data segment, not the kernel 
		 * segment so "*" assignment won't work.  We have to use 
		 * put_user which copies data from the kernel data segment to
		 * the user data segment. 
		 */
		result = put_user(tmp->msg[bytes_read], buffer++);
		if (result) {
			return -EFAULT;
		}
		length--;
		bytes_read++;
	}
	/* 
	 * delete the message from the list 
	 */
	list_del(pos);
	kfree(tmp);
	/* 
	 * Most read functions return the number of bytes put into the buffer
	 */
	return bytes_read;
}
	/* 
	 * Actually put the data into the buffer 
	 */
	if (strlen(msg) + 1 < length)
	    length = strlen(msg) + 1;
	result = copy_to_user(buffer, msg, length);

	if (result > 0) 
	    return -EFAULT; /* copy failed */
	/* 
	 * Most read functions return the number of bytes put into the buffer
	 */
	return length;
}

/* Called when a process writes to dev file: echo "hi" > /dev/hello  */
// Writing to the device stores the message in kernel space and adds it to the list if the message is below the maximum size, and the limit of the number of all messages stored in the kernel  wouldn't be surpassed with this message. If the message is too big, -EINVAL is returned, and if the limit of the number of all messages was surpassed, -EBUSY is returned.

static ssize_t device_write(struct file *filp, const char *buff, size_t len, loff_t * off)
{
	/* 
	 * Number of bytes actually written to the buffer 
	 */
	int bytes_written = 0;
	/* 
	 * Pointer to the current message 
	 */
	struct msg_struct *tmp;
	/* 
	 * Pointer to the current position in the list 
	 */
	struct list_head *pos;
	/* 
	 * If the message is too big, return -EINVAL 
	 */
	if (len > MAX_MSG_SIZE) {
		return -EINVAL;
	}
	/* 
	 * If the limit of the number of all messages was surpassed, return -EBUSY 
	 */
	if (list_size >= MAX_MSG_COUNT) {
		return -EBUSY;
	}
	/* 
	 * Allocate memory for the new message 
	 */
	tmp = kmalloc(sizeof(struct msg_struct), GFP_KERNEL);
	if (!tmp) {
		return -ENOMEM;
	}
	/* 
	 * Copy the message from the user space to the kernel space 
	 */
	if (copy_from_user(tmp->msg, buff, len)) {
		kfree(tmp);
		return -EFAULT;
	}
	/* 
	 * Add the new message to the list 
	 */
	list_add_tail(&tmp->list, &mas_list);
	list_size++;
	/* 
	 * Most write functions return the number of bytes put into the buffer 
	 */
	return len;
}
static ssize_t
device_write(struct file *filp, const char *buff, size_t len, loff_t * off)
{
	printk(KERN_ALERT "Sorry, this operation isn't supported.\n");
	return -EINVAL;
}





