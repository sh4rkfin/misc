#!/usr/bin/env python3

import tiktoken

with open('shakespeare.txt', 'r', encoding='utf-8') as f:
    text = f.read()

first = text[:200]
print(first)

enc = tiktoken.get_encoding("cl100k_base")

encoded = enc.encode(first)
print(encoded)

for i in encoded:
    print(f'{i}:\t\"{enc.decode([i])}\"')

encoded = enc.encode(text)
distinct = set(encoded)
print('# of tokens in text: ', len(encoded))
print('# of distinct encodings: ', len(distinct))
print('max token value: ', enc.max_token_value)
