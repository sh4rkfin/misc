#!/usr/bin/env ruby -w

class Node
  @@depth_invocations=0

  attr_reader :depth_invocations
  def initialize(children)
    @children = children
  end

  def depth
    @@depth_invocations += 1
    if @children.empty?
      return 0
    end
    @children.collect { |c| c.depth() }.max + 1
    # max_depth = 0
    # for node in @children
    # d = node.depth()
    # max_depth = [d, max_depth].max
    # end
    # return max_depth + 1
  end

  def self.depth_invocations
    @@depth_invocations
  end
end

a = Node.new([])
b = Node.new([a])
c = Node.new([a, b])
d = Node.new([a, b, c])

print "depth: ", d.depth, ", depth_invocations: ", Node.depth_invocations(), "\n"