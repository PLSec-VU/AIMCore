#include "secure_memory.h"
#include <unistd.h>
#include <errno.h>
#include <stdint.h>

// Internal function to make the syscall
static int _mark_memory_region_syscall(void *addr, size_t size, int level) {
    // Use inline assembly to make the syscall
    // RISC-V calling convention: a0=addr, a1=size, a2=level, a7=syscall_number
    register long a0 asm("a0") = (long)addr;
    register long a1 asm("a1") = (long)size;
    register long a2 asm("a2") = (long)level;
    register long a7 asm("a7") = SYS_MARK_MEMORY_REGION;
    register long result asm("a0");
    
    asm volatile (
        "ecall"
        : "=r"(result)
        : "r"(a0), "r"(a1), "r"(a2), "r"(a7)
        : "memory"
    );
    
    return (int)result;
}

int mark_memory_region(void *addr, size_t size, int level) {
    if (addr == NULL || size == 0) {
        errno = EINVAL;
        return -1;
    }
    
    if (level != SECURITY_PUBLIC && level != SECURITY_SECRET) {
        errno = EINVAL;
        return -1;
    }
    
    return _mark_memory_region_syscall(addr, size, level);
}

int mark_memory_secret(void *addr, size_t size) {
    return mark_memory_region(addr, size, SECURITY_SECRET);
}

int mark_memory_public(void *addr, size_t size) {
    return mark_memory_region(addr, size, SECURITY_PUBLIC);
}

int secure_stack_init(secure_stack_frame_t *frame, void *addr, size_t size) {
    if (frame == NULL || addr == NULL || size == 0) {
        errno = EINVAL;
        return -1;
    }
    
    // Initialize the frame
    frame->addr = addr;
    frame->size = size;
    frame->original_level = SECURITY_PUBLIC; // Assume originally public
    frame->active = 0;
    
    // Mark the region as secret
    int result = mark_memory_secret(addr, size);
    if (result == 0) {
        frame->active = 1;
    }
    
    return result;
}

void secure_stack_cleanup(secure_stack_frame_t *frame) {
    if (frame == NULL || !frame->active) {
        return;
    }
    
    // Restore the original security level (public)
    mark_memory_public(frame->addr, frame->size);
    frame->active = 0;
}

// Example usage functions for demonstration

/**
 * Example function showing how to use the secure memory API
 * with manual cleanup
 */
void example_manual_cleanup(void) {
    char sensitive_data[256];
    secure_stack_frame_t frame;
    
    // Mark the buffer as secret
    if (secure_stack_init(&frame, sensitive_data, sizeof(sensitive_data)) == 0) {
        // Do sensitive operations with the buffer
        // ...
        
        // Manually cleanup when done
        secure_stack_cleanup(&frame);
    }
}

/**
 * Example function showing how to use the secure memory API
 * with automatic cleanup (GCC only)
 */
void example_automatic_cleanup(void) {
#ifdef __GNUC__
    char sensitive_data[256];
    
    // This will automatically mark as secret and cleanup when function exits
    SECURE_VAR(sensitive_data);
    
    // Do sensitive operations with the buffer
    // The cleanup happens automatically when the function returns
    // or when the variable goes out of scope
#endif
}

/**
 * Example function showing how to secure an array
 */
void example_secure_array(void) {
#ifdef __GNUC__
    uint8_t crypto_keys[32];
    
    // Mark the entire array as secret with automatic cleanup
    SECURE_ARRAY(crypto_keys, 32);
    
    // Do cryptographic operations
    // ...
    
    // Automatic cleanup when function exits
#endif
}