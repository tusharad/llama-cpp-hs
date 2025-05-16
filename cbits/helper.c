#include <llama.h>
#include <stdlib.h>

void _llama_backend_init() {
    llama_backend_init(); 
}

void _llama_backend_free() {
    llama_backend_free();     
}

void llama_batch_init_into(
    int32_t n_tokens,
    int32_t embd,
    int32_t n_seq_max,
    struct llama_batch * out) {
    
    if (out == NULL) return;

    *out = (struct llama_batch){
        .n_tokens = n_tokens,
        .token = NULL,       // Set by Haskell
        .embd = NULL,        // Set by Haskell
        .pos = NULL,         // Set by Haskell
        .n_seq_id = NULL,    // Set by Haskell
        .seq_id = NULL,      // Set by Haskell
        .logits = NULL,      // Set by Haskell
    };

    // Call the original llama_batch_init
    *out = llama_batch_init(n_tokens, embd, n_seq_max);
}

int32_t llama_decode_wrap(struct llama_context * ctx, struct llama_batch *batch) {
    llama_decode(ctx, *batch);
}

void llama_sampler_chain_default_params_into(struct llama_sampler_chain_params * out) {
    *out = llama_sampler_chain_default_params();
}

struct llama_model * llama_model_load_from_file_wrap (const char * path_model, const struct llama_model_params * params) {
    return llama_model_load_from_file(path_model, *params);
}

struct llama_context * llama_init_from_model_into(struct llama_model * model, const struct llama_context_params * params) {
    return llama_init_from_model(model, *params);
}

void llama_batch_free_wrap(struct llama_batch * batch) {
    llama_batch_free(*batch);
}

void llama_batch_get_one_into(llama_token * tokens
            , int32_t n_tokens, struct llama_batch * out_batch) {
    if (out_batch == NULL) {
        return; // invalid output pointer
    }
    *out_batch = llama_batch_get_one(tokens, n_tokens);
}

void llama_pooling_type_into(const struct llama_context * ctx, int * out) {
    *out = llama_pooling_type(ctx);
}

void llama_model_rope_type_into(const struct llama_model * model, int * out) {
    *out = llama_model_rope_type(model);
}

void llama_context_default_params_into(struct llama_context_params* out) {
    if (out != NULL) {
        *out = llama_context_default_params();
    }
}

void llama_kv_cache_view_init_into(const struct llama_context * ctx, int32_t n_seq_max, struct llama_kv_cache_view* out) {
    if (out != NULL) {
        *out = llama_kv_cache_view_init(ctx, n_seq_max);
    }
}

void llama_perf_context_into(const struct llama_context * ctx, struct llama_perf_context_data* out ) {
    if (out != NULL) {
        *out = llama_perf_context(ctx);
    }
}

void llama_perf_sampler_into(const struct llama_sampler * chain, struct llama_perf_sampler_data* out) {
    if (out != NULL) {
        *out = llama_perf_sampler(chain);
    }
}
