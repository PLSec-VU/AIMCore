#ifndef SECURE_MEMORY_H
#define SECURE_MEMORY_H

#include <stddef.h>
#include <stdint.h>

// Security levels
#define SECURITY_PUBLIC 0
#define SECURITY_SECRET 1

// Syscall number for memory region marking
#define SYS_MARK_MEMORY_REGION 676767

/**
 * Mark a memory region with a specific security level
 * @param addr Start address of the memory region
 * @param size Size of the memory region in bytes
 * @param level Security level (SECURITY_PUBLIC or SECURITY_SECRET)
 * @return 0 on success, -1 on error
 */
int mark_memory_region(void *addr, size_t size, int level);

/**
 * Mark a memory region as secret
 * @param addr Start address of the memory region
 * @param size Size of the memory region in bytes
 * @return 0 on success, -1 on error
 */
int mark_memory_secret(void *addr, size_t size);

/**
 * Mark a memory region as public
 * @param addr Start address of the memory region
 * @param size Size of the memory region in bytes
 * @return 0 on success, -1 on error
 */
int mark_memory_public(void *addr, size_t size);

/**
 * Secure stack frame structure for automatic cleanup
 * This should be allocated on the stack and will automatically
 * restore memory regions to public when the stack frame is deallocated
 */
typedef struct {
    void *addr;
    size_t size;
    int original_level;
    int active;
} secure_stack_frame_t;

/**
 * Initialize a secure stack frame and mark the region as secret
 * @param frame Pointer to the stack frame structure
 * @param addr Start address of the memory region
 * @param size Size of the memory region in bytes
 * @return 0 on success, -1 on error
 */
int secure_stack_init(secure_stack_frame_t *frame, void *addr, size_t size);

/**
 * Cleanup a secure stack frame and restore the original security level
 * @param frame Pointer to the stack frame structure
 */
void secure_stack_cleanup(secure_stack_frame_t *frame);

/**
 * Macro for automatic cleanup using GCC's cleanup attribute
 * Usage: SECURE_STACK_AUTO(frame, buffer, sizeof(buffer));
 */
#ifdef __GNUC__
#define SECURE_STACK_AUTO(frame, addr, size) \
    secure_stack_frame_t frame __attribute__((cleanup(secure_stack_cleanup))); \
    secure_stack_init(&frame, addr, size)
#else
#define SECURE_STACK_AUTO(frame, addr, size) \
    secure_stack_frame_t frame; \
    secure_stack_init(&frame, addr, size)
#endif

/**
 * Convenience macro for marking a local variable as secret with automatic cleanup
 * Usage: SECURE_VAR(my_secret_buffer);
 */
#define SECURE_VAR(var) \
    SECURE_STACK_AUTO(__secure_##var, &(var), sizeof(var))

/**
 * Convenience macro for marking an array as secret with automatic cleanup
 * Usage: SECURE_ARRAY(my_secret_array, element_count);
 */
#define SECURE_ARRAY(var, count) \
    SECURE_STACK_AUTO(__secure_##var, (var), sizeof((var)[0]) * (count))

#endif // SECURE_MEMORY_H