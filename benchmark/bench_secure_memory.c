#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "secure_memory.h"

// Test function demonstrating manual security management
void test_manual_security(void) {
    printf("=== Testing Manual Security Management ===\n");
    
    char secret_buffer[64];
    strcpy(secret_buffer, "This is sensitive data that should be protected");
    
    printf("Buffer contents: %s\n", secret_buffer);
    printf("Buffer address: %p, size: %zu\n", secret_buffer, sizeof(secret_buffer));
    
    // Mark the buffer as secret
    if (mark_memory_secret(secret_buffer, sizeof(secret_buffer)) == 0) {
        printf("Successfully marked buffer as SECRET\n");
        
        // Do some sensitive operations
        for (int i = 0; i < 10; i++) {
            secret_buffer[i] ^= 0xAA; // Simple encryption
        }
        
        // Mark it back as public when done
        if (mark_memory_public(secret_buffer, sizeof(secret_buffer)) == 0) {
            printf("Successfully restored buffer to PUBLIC\n");
        } else {
            printf("Failed to restore buffer to PUBLIC\n");
        }
    } else {
        printf("Failed to mark buffer as SECRET\n");
    }
}

// Test function demonstrating automatic cleanup with stack frames
void test_stack_frame_security(void) {
    printf("\n=== Testing Stack Frame Security ===\n");
    
    char sensitive_data[128];
    secure_stack_frame_t frame;
    
    strcpy(sensitive_data, "Highly confidential information");
    printf("Data: %s\n", sensitive_data);
    printf("Address: %p, size: %zu\n", sensitive_data, sizeof(sensitive_data));
    
    // Initialize secure stack frame (marks as secret)
    if (secure_stack_init(&frame, sensitive_data, sizeof(sensitive_data)) == 0) {
        printf("Successfully initialized secure stack frame\n");
        
        // Perform sensitive operations
        for (size_t i = 0; i < strlen(sensitive_data); i++) {
            sensitive_data[i] = ((sensitive_data[i] + 1) % 256);
        }
        
        printf("Processed sensitive data\n");
        
        // Manual cleanup (normally this would be automatic)
        secure_stack_cleanup(&frame);
        printf("Stack frame cleaned up - memory restored to PUBLIC\n");
    } else {
        printf("Failed to initialize secure stack frame\n");
    }
}

// Test function demonstrating automatic cleanup with macros (GCC only)
void test_automatic_cleanup(void) {
    printf("\n=== Testing Automatic Cleanup (GCC) ===\n");
    
#ifdef __GNUC__
    {
        uint8_t crypto_key[32];
        
        // Fill with dummy key data
        for (int i = 0; i < 32; i++) {
            crypto_key[i] = i * 7 + 13; // Some pattern
        }
        
        printf("Crypto key address: %p\n", crypto_key);
        
        // This macro automatically marks as secret and sets up cleanup
        SECURE_VAR(crypto_key);
        
        printf("Crypto key marked as SECRET with automatic cleanup\n");
        
        // Simulate cryptographic operations
        for (int i = 0; i < 32; i++) {
            crypto_key[i] ^= 0x5A;
        }
        
        printf("Performed crypto operations\n");
        
        // When this scope ends, the cleanup function is automatically called
    }
    printf("Scope ended - automatic cleanup should have restored memory to PUBLIC\n");
#else
    printf("Automatic cleanup requires GCC compiler\n");
#endif
}

// Test function for array security
void test_array_security(void) {
    printf("\n=== Testing Array Security ===\n");
    
    uint32_t sensitive_array[16];
    
    // Initialize array with sensitive data
    for (int i = 0; i < 16; i++) {
        sensitive_array[i] = 0xDEADBEEF + i;
    }
    
    printf("Array address: %p, total size: %zu bytes\n", 
           sensitive_array, sizeof(sensitive_array));
    
#ifdef __GNUC__
    // Mark entire array as secret with automatic cleanup
    SECURE_ARRAY(sensitive_array, 16);
    printf("Array marked as SECRET with automatic cleanup\n");
    
    // Process the array
    for (int i = 0; i < 16; i++) {
        sensitive_array[i] = sensitive_array[i] ^ 0x12345678;
    }
    
    printf("Array processing completed\n");
    // Automatic cleanup happens when function exits
#else
    // Manual approach for non-GCC compilers
    if (mark_memory_secret(sensitive_array, sizeof(sensitive_array)) == 0) {
        printf("Array marked as SECRET\n");
        
        // Process the array
        for (int i = 0; i < 16; i++) {
            sensitive_array[i] = sensitive_array[i] ^ 0x12345678;
        }
        
        mark_memory_public(sensitive_array, sizeof(sensitive_array));
        printf("Array restored to PUBLIC\n");
    }
#endif
}

// Test error handling
void test_error_handling(void) {
    printf("\n=== Testing Error Handling ===\n");
    
    // Test with NULL pointer
    if (mark_memory_secret(NULL, 100) != 0) {
        printf("✓ Correctly rejected NULL pointer\n");
    } else {
        printf("✗ Failed to reject NULL pointer\n");
    }
    
    // Test with zero size
    char buffer[10];
    if (mark_memory_secret(buffer, 0) != 0) {
        printf("✓ Correctly rejected zero size\n");
    } else {
        printf("✗ Failed to reject zero size\n");
    }
    
    // Test with invalid security level
    if (mark_memory_region(buffer, sizeof(buffer), 999) != 0) {
        printf("✓ Correctly rejected invalid security level\n");
    } else {
        printf("✗ Failed to reject invalid security level\n");
    }
}

int main(void) {
    printf("Secure Memory API Test Program\n");
    printf("==============================\n");
    
    test_manual_security();
    test_stack_frame_security();
    test_automatic_cleanup();
    test_array_security();
    test_error_handling();
    
    printf("\n=== All Tests Completed ===\n");
    return 0;
}