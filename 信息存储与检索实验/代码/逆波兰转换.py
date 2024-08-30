import time

def is_operator(char):
    operators = set(['+', '-', '*', '/'])
    return char in operators

def precedence(operator):
    if operator == '+' or operator == '-':
        return 1
    elif operator == '*' or operator == '/':
        return 2
    return 0

def infix_to_postfix(expression):
    stack = []
    postfix = []

    for char in expression:
        if char.isalpha():
            postfix.append(char)
        elif is_operator(char):
            while stack and is_operator(stack[-1]) and precedence(char) <= precedence(stack[-1]):
                postfix.append(stack.pop())
            stack.append(char)
        elif char == '(':
            stack.append(char)
        elif char == ')':
            while stack and stack[-1] != '(':
                postfix.append(stack.pop())
            if stack and stack[-1] == '(':
                stack.pop()

    while stack:
        postfix.append(stack.pop())

    return ''.join(postfix)

infix_expression = input("请输入字母表达式：")

start_time = time.time()
postfix_expression = infix_to_postfix(infix_expression)
end_time = time.time()

print("逆波兰表达式结果：", postfix_expression)

execution_time = end_time - start_time
print("运行时间：", execution_time, "秒")

