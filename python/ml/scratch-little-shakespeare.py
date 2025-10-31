#!/usr/bin/env python3

import torch
import torch.nn as nn
from torch.nn import functional as F

with open('shakespeare.txt', 'r', encoding='utf-8') as f:
    text = f.read()

print("length of dataset in characters: ", len(text))

# print(text[:1000])

chars = sorted(list(set(text)))
vocab_size = len(chars)
print('distinct characters: ' + ''.join(chars))
print('vocab size (number of distinct chars', vocab_size)

# create a mapping from characters to integers
stoi = {ch: i for i, ch in enumerate(chars)}
itos = {i: ch for i, ch in enumerate(chars)}


def encode(s):
    return [stoi[c] for c in s]


def decode(array):
    return ''.join([itos[i] for i in array])


# encode = lambda s: [stoi[c] for c in s]  # encoder: take a string, output a list of integers
# decode = lambda l: ''.join([itos[i] for i in l])  # decoder: take a list of integers, output a string

# print(encode("hii there"))
# print(decode(encode("hii there")))

# let's now encode the entire text dataset and store it into a torch.Tensor
data = torch.tensor(encode(text), dtype=torch.long)
print(data.shape, data.dtype)
print(data[:100])  # the 1000 characters we looked at earlier will to the GPT look like this

# Let's now split up the data into train and validation sets
n = int(0.9*len(data))  # first 90% will be train, rest val
train_data = data[:n]
val_data = data[n:]

block_size = 8
print(f'block size: {block_size}')
print('first block_size tokens of training data:', train_data[:block_size+1])

x = train_data[:block_size]
y = train_data[1:block_size+1]
for t in range(block_size):
    context = x[:t+1]
    target = y[t]
    print(f"when input is {context} the target: {target}")


torch.manual_seed(1337)
batch_size = 4  # how many independent sequences will we process in parallel?
block_size = 8  # what is the maximum context length for predictions?


def get_batch(split):
    # generate a small batch of data of inputs x and targets y
    data = train_data if split == 'train' else val_data
    # returns a 1-tensor of size batch_size random integers
    ix = torch.randint(len(data) - block_size, (batch_size,))
    x = torch.stack([data[i:i+block_size] for i in ix])
    y = torch.stack([data[i+1:i+block_size+1] for i in ix])
    return x, y


xb, yb = get_batch('train')
print('inputs:')
print(xb.shape)
print(xb)
print('targets:')
print(yb.shape)
print(yb)

print('----')

for b in range(batch_size): # batch dimension
    for t in range(block_size): # time dimension
        context = xb[b, :t+1]
        target = yb[b,t]
        print(f"when input is {context.tolist()} the target: {target}")

print(xb)  # our input to the transformer

# reset the seed
torch.manual_seed(1337)


class BigramLanguageModel(nn.Module):

    def __init__(self, vocab_size):
        super().__init__()
        # each token directly reads off the logits for the next token from a lookup table
        # embedding table is a table of vocab_size (65) 1-tensors of size vocab-size
        self.token_embedding_table = nn.Embedding(vocab_size, vocab_size)

    def forward(self, idx, targets=None):

        # idx and targets are both (B,T) tensor of integers
        logits = self.token_embedding_table(idx)  # (B,T,C)
        # logit is a B x T x C tensor where each logits[m,n] tensor
        # (which is 1-tensor of size C) are the "logits" representing
        # the probabilities of the next token given the input token is
        # idx[m,n]

        if targets is None:
            loss = None
        else:
            B, T, C = logits.shape
            logits = logits.view(B*T, C)
            targets = targets.view(B*T)
            loss = F.cross_entropy(logits, targets)

        return logits, loss

    def generate(self, idx, max_new_tokens):
        # idx is (B, T) array of indices in the current context
        for _ in range(max_new_tokens):
            # get the predictions
            logits, _ = self(idx)
            # focus only on the last time step
            logits = logits[:, -1, :]  # becomes (B, C)
            # apply softmax to get probabilities
            probs = F.softmax(logits, dim=-1)  # (B, C)
            # sample from the distribution
            idx_next = torch.multinomial(probs, num_samples=1) # (B, 1)
            # append sampled index to the running sequence
            idx = torch.cat((idx, idx_next), dim=1) # (B, T+1)
        return idx


model = BigramLanguageModel(vocab_size)
logits, loss = model(xb, yb)
print(logits.shape)
print(loss)

idx = torch.zeros((1, 1), dtype=torch.long)  # 2-tensor (1, 1) of integer zeros
generated = model.generate(idx=idx, max_new_tokens=100)
print(decode(generated[0].tolist()))

print('number of parameters: ', sum(p.numel() for p in model.parameters() if p.requires_grad))
print(f'vocab_size * vocab_size')

# create a PyTorch optimizer
optimizer = torch.optim.AdamW(model.parameters(), lr=1e-3)

batch_size = 32
for steps in range(40000):  # increase number of steps for good results...

    # sample a batch of data
    xb, yb = get_batch('train')

    # evaluate the loss
    logits, loss = model(xb, yb)
    optimizer.zero_grad(set_to_none=True)
    loss.backward()
    if steps % 1000 == 0:
        print(f'Step: {steps}, loss: {loss}')
    optimizer.step()

print(loss.item())

idx = torch.zeros((1, 1), dtype=torch.long)  # 2-tensor (1, 1) of integer zeros
generated = model.generate(idx=idx, max_new_tokens=100)
print(decode(generated[0].tolist()))
