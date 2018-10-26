TARGET=/usr/local/lib/liblinenjni.so
SOURCE_DIR=src/native
BUILD_DIR=target
INCLUDES_DIR=/usr/local/include/linenjni
BIN_DIR=$(BUILD_DIR)/bin
OBJ_DIR=$(BUILD_DIR)/obj
CC=clang
CFLAGS=-Wall -O2 -fPIC -I/usr/lib/jvm/default/include -I/usr/lib/jvm/default/include/linux -I./src/native
JAVA_SOURCES=${wildcard $(SOURCE_DIR)/*.java}
HEADERS=$(JAVA_SOURCES:%.java=%.h)
SOURCES=${wildcard $(SOURCE_DIR)/*.c}
OBJECTS=$(notdir $(SOURCES:%.c=%.o))
OBJECTS:=$(OBJECTS:%=$(OBJ_DIR)/%)
LIBS=-lssh
SUDO=

default=$(BIN_DIR)/liblinenjni.so

$(BIN_DIR)/liblinenjni.so: $(OBJECTS) $(BIN_DIR)
	$(CC) -shared $(LIBS) $(OBJECTS) -o $@ -s

$(BIN_DIR):
	mkdir $(BIN_DIR)

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

$(OBJ_DIR)/%.o: $(SOURCE_DIR)/%.c $(OBJ_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(INCLUDES_DIR)/:
	$(SUDO) mkdir -p /usr/local/include/linenjni

$(SOURCE_DIR)/%.h: $(SOURCE_DIR)/%.java $(BUILD_DIR)
	javac -h $(SOURCE_DIR) -d $(BUILD_DIR) $<

$(INCLUDES_DIR)/%.h: $(wildcard $(SOURCE_DIR)/*.h) $(INCLUDES_DIR)
	$(SUDO) cp $< $@;

install: $(BIN_DIR)/liblinenjni.so
	$(SUDO) cp $(BIN_DIR)/liblinenjni.so $(TARGET)

uninstall:
	$(SUDO) rm -f $(TARGET)
	$(SUDO) rm -rf /usr/local/include/linenjni

clean:
	rm -rf $(BIN_DIR) $(OBJ_DIR)
