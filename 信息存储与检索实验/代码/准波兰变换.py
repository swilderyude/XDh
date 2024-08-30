import time

class Node:
    def __init__(self, value):
        self.value = value
        self.left = None
        self.right = None

def is_operator(char):
    operators = set(['+', '-', '*', '/'])
    return char in operators

def build_expression_tree(expression):
    stack = []

    for char in expression:
        if char.isalpha():
            node = Node(char)
            stack.append(node)
        elif is_operator(char):
            if len(stack) < 2:
                return None
            node = Node(char)
            node.right = stack.pop()
            node.left = stack.pop()
            stack.append(node)
        else:
            return None

    return stack.pop()

def prefix_traversal(node):
    if node is not None:
        print(node.value, end=' ')
        prefix_traversal(node.left)
        prefix_traversal(node.right)

def infix_to_prefix(expression):
    precedence = {'+': 1, '-': 1, '*': 2, '/': 2, '^': 3}
    operators = set(precedence.keys())
    stack = []
    output = []
  
    for char in expression:
        if char.isalpha():  # 如果是字母，直接添加到输出
            output.append(char)
        elif char == '(':
            stack.append(char)
        elif char == ')':
            while stack and stack[-1] != '(':
                output.append(stack.pop())
            stack.pop()  # 弹出'('
        elif char in operators:  # 操作符
            while stack and stack[-1] in operators and precedence[char] <= precedence[stack[-1]]:
                output.append(stack.pop())
            stack.append(char)
        else:
            return None

    while stack:
        if stack[-1] == '(':
            print("表达式错误")
            return None
        output.append(stack.pop())

    return ' '.join(output)

expression = input("请输入字母表达式：")
prefix_expression = infix_to_prefix(expression)

if prefix_expression is not None:
    print("准波兰表达式:", prefix_expression)
    root = build_expression_tree(prefix_expression)
    if root is not None:
        print("准波兰变换法（二叉树）结果：")
        prefix_traversal(root)
 
start_time = time.time()
root = build_expression_tree(expression)
end_time = time.time()
       
execution_time = end_time - start_time
print("运行时间：", execution_time, "秒")




